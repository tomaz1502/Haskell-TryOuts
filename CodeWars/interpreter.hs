import qualified Data.Map.Strict as M
import Data.Char (isLower)
import Data.Maybe

type Registers = M.Map String Int

data Instruction = MkArith ArithInstruction | MkJump JumpInstruction
data JumpInstruction = Jnz String RegOrConstant
data ArithInstruction = Mov String RegOrConstant | Inc String | Dec String
data RegOrConstant = Reg String | Cons Int

apply :: ArithInstruction -> Registers -> Registers
apply (Mov s (Cons x)) regs = M.alter (const (Just x)) s regs
apply (Mov s (Reg r))  regs = M.alter (const (M.lookup r regs)) s regs
apply (Inc s)          regs = M.alter (fmap (+ 1)) s regs
apply (Dec s)          regs = M.alter (fmap (\r -> r - 1)) s regs

getOps :: String -> RegOrConstant
getOps s
    | isLower (head s) = Reg s
    | otherwise        = Cons (read s :: Int)

convert :: String -> Instruction
convert s
    | name == "mov" = MkArith (Mov op1 op2)
    | name == "inc" = MkArith (Inc op1)
    | name == "dec" = MkArith (Dec op1)
    | otherwise     = MkJump  (Jnz op1 op2)
    where
        [name, op1] = take 2 (words s)
        op2s = drop 2 (words s)
        op2 = getOps (head op2s)


val :: Registers -> RegOrConstant -> Int
val _    (Cons x) = x
val regs (Reg s)  = fromJust $ M.lookup s regs

go :: Int -> Registers -> [String] -> [String] -> Registers
go _ regs [] _ = regs
go i regs (s : ss) insts = case convert s of
    MkJump (Jnz s op) -> if M.lookup s regs == Just 0
                            then go (i + 1) regs ss insts
                            else go (i + step) regs (drop (i + step) insts) insts
        where step = val regs op
    MkArith ai       -> go (i + 1) (apply ai regs) ss insts

simpleAssembler :: [String] -> Registers
simpleAssembler s = go 0 M.empty s s

main :: IO ()
main = print $ show $ simpleAssembler ["bla"]
