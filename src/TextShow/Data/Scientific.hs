module TextShow.Data.Scientific () where
import           Data.Scientific
import           Data.Text.Lazy.Builder.Scientific
import           TextShow

instance TextShow Scientific where
    showb = scientificBuilder
