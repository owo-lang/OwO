module OwO.Syntax.Abstract where

-- | Program Structure Item: File Type
data PsiFileType
  = CodeFileType
  | LiterateFileType
  deriving (Eq, Ord, Show)

-- | Qualified Name
data QName' str
  = QLocalName str
  | str |.| QName' str
  deriving (Eq, Ord, Show)

-- | Specialized to String for convenience
type QName = QName' String

-- | Program Structure Item: File
data PsiFile = PsiFile
  { fileType           :: PsiFileType
  ; topLevelModuleName :: QName
  } deriving (Eq, Show)


