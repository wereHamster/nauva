module Nauva.App
    ( AppH(..)
    , App(..)

    , module Nauva.Internal.Types
    , module Nauva.Service.Head
    , module Nauva.Service.Router
    ) where


import Nauva.Internal.Types (Element(..))

import Nauva.Service.Head
import Nauva.Service.Router


data AppH = AppH
    { headH :: !HeadH
    , routerH :: !RouterH
    }


data App = App
    { rootElement :: AppH -> Element
    }
