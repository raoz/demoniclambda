module AST where

import TreeSitter.Api
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.C
import Foreign.Storable
import Data.Word
import Data.Either
import Control.Monad



data Term = Variable String
          | Abstraction String Term
          | Application Term Term
          | Number Integer
          | BinOp String Term Term
          | Boolean Bool deriving (Eq)

lambda = "λ"
comment = "―"

instance Show Term
        where
                show (Variable v) = v
                show (Abstraction v t) = lambda <> v <> "." <> show t
                show (Application t1 t2) = "(" <> show t1 <> ") (" <> show t2 <> ")"
                show (Number n) = show n
                show (Boolean True) = "⊤"
                show (Boolean False) = "⊥"
                show (BinOp op t1 t2) = "(" <> show t1 <> " " <> op <> " " <> show t2 <> ")"

data TermInfo = TermInfo{
        termInfo'child_list :: [String],
        termInfo'get_term :: String -> [Either String Term] -> Either String Term
}
termInfo :: String -> TermInfo
termInfo "paren_term" = TermInfo
        ["(", "expression in parentheses", ")"]
        $ \_ (_:x:_) -> x
termInfo "abstraction" = TermInfo
        ["λ", "bound variable", ".", "abstraction righthand value"]
        $ \_ (_:Left bound:_:Right term:_) -> Right $ Abstraction bound term
termInfo "application" = TermInfo
        ["appliable function", "application argument"]
        $ \_ (Right left:Right right:_) -> Right $ Application left right
termInfo "variable" = TermInfo
        []
        $ \s _ -> Right $ Variable s
termInfo "bin_op" = TermInfo
        ["lefthand expr of bin op", "bin op", "righthand expr of bin op"]
        $ \_ (Right left:Left operator:Right right:_) -> Right $ BinOp operator left right
termInfo "number" = TermInfo
        []
        $ \s _ -> Right . Number $ read s
termInfo "boolean" = TermInfo
        []
        $ \s _ -> case s of
                        "⊤" -> Right $ Boolean True
                        "⊥" -> Right $ Boolean False
termInfo "assignment" = TermInfo
        ["assignable variable", "≔", "assignment value", "\\n"]
        $ \_ (Left bound:_:Right term:_) -> Right $ Application (Abstraction bound (Variable "MISSING")) term
termInfo "program" = TermInfo
        (repeat "statement-ish")
        $ \_ stmt_list -> foldr1 (\(Right (Application (Abstraction bound _) term)) (Right main) ->
                Right $ Application (Abstraction bound main) term
        ) stmt_list

termInfo _ = TermInfo [] $ \s _ -> Left s


type SourceName = String
type Line       = Int
type Column     = Int
data Message = Expect String | UnExpect String
        deriving (Show)
data SourceLoc = SourceLoc SourceName Line Column Line Column
        deriving (Show)
data ParseError = ParseError Message SourceLoc
        deriving (Show)


substr :: Ptr CChar -> Ptr C'TSNode -> IO String
substr s node = do
        start_byte <- c'hs_node_start_byte node
        end_byte <- c'hs_node_end_byte node
        peekCStringLen (plusPtr s $ fromIntegral start_byte, fromIntegral $ end_byte - start_byte)

formatError :: Ptr C'TSNode -> Message -> IO ParseError
formatError node message = alloca $ \start_point_ptr -> alloca $ \end_point_ptr -> do
        c'hs_node_start_point node start_point_ptr
        start_point <- peek start_point_ptr

        c'hs_node_end_point node end_point_ptr
        end_point <- peek end_point_ptr

        return $ ParseError message
                (SourceLoc "<stdin>" (fromIntegral $ c'TSPoint'row start_point)
                                     (fromIntegral $ c'TSPoint'column start_point)
                                     (fromIntegral $ c'TSPoint'row end_point)
                                     (fromIntegral $ c'TSPoint'column end_point))

convertTree:: Ptr C'TSNode -> String -> Ptr CChar -> IO (Either [ParseError] (Either String Term))
convertTree node expected s = do
        is_missing <- c'hs_node_is_missing node
        if is_missing /= 0
        then do
                parseError <- formatError node $ Expect expected
                return . Left $ [parseError]
        else alloca $ \first_child -> do
                node_type_ptr <- c'hs_node_type node
                node_type <- peekCString node_type_ptr

                c'hs_node_child node 0 first_child
                sibling_info <- iterateSiblings first_child (termInfo'child_list $ termInfo node_type) s
                case sibling_info of
                        Left error_list -> return $ Left error_list
                        Right term_list -> do
                                ss <- substr s node
                                return . Right $ (termInfo'get_term $ termInfo node_type) ss term_list

iterateSiblings:: Ptr C'TSNode -> [String] -> Ptr CChar -> IO (Either [ParseError] [Either String Term])
iterateSiblings _ [] _ = return $ Right []
iterateSiblings node (expected:plan) s = do
        is_null <- c'hs_node_is_null node
        if is_null /= 0
        then return $ Right []
        else alloca $ \next_sibling -> do
                c'hs_node_next_sibling node next_sibling

                node_type_ptr <- c'hs_node_type node
                node_type <- peekCString node_type_ptr
                if node_type == "ERROR"
                then do
                        ss <- substr s node
                        parseError <- formatError node $ UnExpect ss

                        following <- iterateSiblings next_sibling (expected:plan) s
                        return . Left $ parseError : fromLeft [] following
                else do
                        current <- convertTree node expected s
                        following <- iterateSiblings next_sibling plan s
                        case current of
                                Left current -> return . Left $ current ++ fromLeft [] following
                                Right current -> case following of
                                                Left following -> return . Left $ following
                                                Right following -> return . Right $ current : following



foreign import ccall unsafe "vendor/tree-sitter-demoniclambda/src/parser.c tree_sitter_demoniclambda" c'tree_sitter_demoniclambda :: Ptr C'TSLanguage

parseTerm :: String -> IO(Either [ParseError] Term)
parseTerm s =  alloca $ \root_node -> do
        parser <- c'ts_parser_new
        c'ts_parser_set_language parser c'tree_sitter_demoniclambda

        (str, len) <- newCStringLen s
        tree <- c'ts_parser_parse_string parser nullPtr str (fromIntegral len)
        c'hs_tree_root_node tree root_node

        result <- convertTree root_node "program" str
        case result of
                Left error_list -> return $ Left error_list
                Right maybe_term -> case maybe_term of
                        Left string -> error "WTF?"
                        Right term -> return $ Right term
