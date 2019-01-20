

import ParserImp
import Data.Map

type State = (Map String Integer)
type Cfg = ([Stmt], State)


stop :: Cfg -> Cfg
stop (((AVal x) 'Plus' (AVal y)):rest, 

