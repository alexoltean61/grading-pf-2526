{-
    În acest modul sunt definite tipurile de date pe baza cărora vă este calculat punctajul.
-}
module Model where

type Laborator   = Integer
type StudentPair = (Student, DateStudent)

data Student = Student
    {   nume     :: String
    ,   prenume  :: String
    ,   grupa    :: String
    } deriving Show

data DateStudent = DateStudent
    {
    -- | Numărul de prezențe la laborator   
        prezente     :: Integer
    -- | De câte ori ați ieșit în față să rezolvați o problemă
    ,   iesiri       :: Integer
    -- | Pentru fiecare laborator, numele exercițiilor rezolvate de voi,
    --   reținute ca Maybe String.
    --   O valoare Nothing la un lab indică suspiciune de LLM.
    ,   ex           :: [(Laborator, Maybe String)]
    -- | Pentru fiecare laborator, punctajul total obținut pe baza exercițiilor.
    --   Calculat ca: (nr. ex. rezolvate) / (total ex. lab)
    ,   punctajEx    :: [(Laborator, Double)]
    -- | Pentru fiecare laborator, numele exercițiilor bonus rezolvate de voi,
    --   reținute ca Maybe String.
    --   O valoare Nothing la un lab indică suspiciune de LLM.
    ,   bonus        :: [(Laborator, Maybe String)]
    -- | Pentru fiecare laborator, punctajul total obținut pe baza bonusurilor.
    --   Calculat ca: (nr. bonus rezolvate) / (total bonus lab)
    ,   punctajBonus :: [(Laborator, Double)]
    } deriving Show

studentModel :: Laborator -> StudentPair
studentModel nrLab = (Student { nume = "Student", prenume = "Premiant", grupa = "nr 1" },
    DateStudent { prezente = nrLab, iesiri = nrLab-1, ex = [], bonus = [], punctajEx = zip [2 .. nrLab] (repeat 1), punctajBonus = zip [2 .. nrLab] (repeat 1) } )
