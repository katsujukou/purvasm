-- | A decoded CoreFn module (ADR-0014), the full record from the reference. Some
-- | fields (path, built_with, exports, re_exports) are retained for fidelity even
-- | though lowering may not use them yet.
module PureScript.CoreFn.Module where

import Foreign.Object (Object)
import PureScript.CoreFn.Ann (Ann)
import PureScript.CoreFn.Expr (Bind)
import PureScript.CoreFn.Names (ModuleName, Ident)

-- | An imported module reference.
type Import = { ann :: Ann, moduleName :: ModuleName }

-- | A decoded CoreFn module.
type Module =
  { name :: ModuleName
  , path :: String
  , builtWith :: String
  , imports :: Array Import
  , exports :: Array Ident
  , reExports :: Object (Array Ident)
  , foreignNames :: Array Ident
  , decls :: Array Bind
  }
