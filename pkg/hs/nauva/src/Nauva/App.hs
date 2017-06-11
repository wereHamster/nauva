module Nauva.App
    ( AppH(..)
    , App(..)

    , HeadH(..)
    , constHead

    , RouterH(..)
    ) where


import Nauva.Internal.Types

import Nauva.Service.Head
import Nauva.Service.Router


data AppH = AppH
    { headH :: !HeadH
    , routerH :: !RouterH
    }


data App = App
    { rootElement :: AppH -> Element
    }
