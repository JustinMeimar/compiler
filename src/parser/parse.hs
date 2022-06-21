-- TODO IMport getToken Function

data AbstractSyntaxTree tok = Empty 
    | Leaf tok
    | Node (AbstractSyntaxTree tok) tok (AbstractSyntaxTree tok)

