-- vim: set fileencoding=utf-8 :
--
--       Author:   Takahiro Oshima <tarotora51@gmail.com>
--       License:  MIT License
--       Created:  2015-03-13
--

import Control.Applicative
import Control.Monad
import Data.List

employIncomeDedu x 
    | x <= 65.0 = 0.0
    | x <= 180.0 = x * 0.4
    | x <= 360.0 = x * 0.3 + 18.0
    | x <= 660.0 = x * 0.2 + 54.0
    | x <= 1000.0 = x * 0.1 + 120.0
    | x <= 1500.0 = x * 0.05 + 170.0
    | x > 1500.0 = 245.0

baseDedu = 38.0

taxExempForDepend = 0 
-- 38 63 58
 
taxableAmount income = income - ( (employIncomeDedu income) + baseDedu + taxExempForDepend )

baseIncomeTax x
    | x <= 195.0 = x * 0.05
    | x <= 330.0 = x * 0.1 - 9.75
    | x <= 695.0 = x * 0.2 - 42.75
    | x <= 900.0 = x * 0.23 - 63.6
    | x <= 1800.0 = x * 0.33 - 153.6
    | x > 1800.0 = x * 0.4 - 279.6

specialTaxForHUKKOU x = x * 0.021

tax income = baseIncomeTax $ taxableAmount income + (specialTaxForHUKKOU $ baseIncomeTax $ taxableAmount income)

realIncome income = income - (tax income)

printTaxList lower upper = mapM_ print $ map tax [lower .. upper]

