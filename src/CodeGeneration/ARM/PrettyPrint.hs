module CodeGeneration.ARM.PrettyPrint (showArm) where

import CodeGeneration.ARM.Registers (ArmInstrs)
import CodeGeneration.IR (Instrs)

import qualified Data.Text as T

showArm :: ArmInstrs -> T.Text
showArm = undefined