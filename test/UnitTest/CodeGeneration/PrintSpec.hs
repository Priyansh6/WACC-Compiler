{-# LANGUAGE OverloadedStrings #-}

module UnitTest.CodeGeneration.PrintSpec (spec) where

import CodeGeneration.ARM.PrettyPrint
import CodeGeneration.ARM.Registers
import CodeGeneration.IR
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = do
  it "adds labels and values to .data" $
    showArm
      [ Section
          [ StringData "._s" "bob bill3",
            StringData "a2_" ""
          ]
          (Body "main" False [])
      ]
      `shouldBe` T.unlines
        [ ".data",
          "\t.word 9",
          "._s:",
          "\t.asciz \"bob bill3\"",
          "\t.word 0",
          "a2_:",
          "\t.asciz \"\"",
          ".text",
          "main:"
        ]

  it "creates functions" $
    showArm
      [ Section
          []
          (Body "func1" True [Define "func2"])
      ]
      `shouldBe` T.unlines
        [ ".text",
          ".global func1",
          "func1:",
          "func2:"
        ]

  it "show arithmetic instructions" $
    showArm
      [ Section
          []
          ( Body
              "main"
              True
              [ Add (Reg R0) (Reg R8) (Reg R9),
                Mul (Reg R0) (Reg R9) (Reg R4)
              ]
          )
      ]
      `shouldBe` T.unlines
        [ ".text",
          ".global main",
          "main:",
          "\tadds r0, r8, r9",
          "\tsmull r0, r9, r9, r4"
        ]

  it "pushes and pops" $
    showArm
      [ Section
          []
          ( Body
              "main"
              True
              [ Push (Regs [LR]),
                Pop (Regs [R8, R10, R12]),
                Pop (Regs [FP])
              ]
          )
      ]
      `shouldBe` T.unlines
        [ ".text",
          ".global main",
          "main:",
          "\tpush {lr}",
          "\tpop {r8, r10, r12}",
          "\tpop {fp}"
        ]

  it "comments" $
    showArm
      [ Section
          []
          ( Body
              "asdf"
              False
              [ Comment "hello there",
                Cmp (Ind R8) (Imm (-1)),
                Comment "hi@",
                Define "yo",
                Comment "@hello@",
                Jsr "yo"
              ]
          )
      ]
      `shouldBe` T.unlines
        [ ".text",
          "asdf:",
          "\t@ hello there",
          "\tcmp [r8], #-1",
          "\t@ hi@",
          "yo:",
          "\t@ @hello@",
          "\tbl yo"
        ]

  it "loads different address modes" $
    showArm
      [ Section
          []
          ( Body
              "main"
              True
              [ Load (Reg R0) (Abs ".L._println_str0"),
                Load (Reg R1) (ImmOffset R0 (-4))
              ]
          )
      ]
      `shouldBe` T.unlines
        [ ".text",
          ".global main",
          "main:",
          "\tldr r0, =.L._println_str0",
          "\tldr r1, [r0, #-4]"
        ]

  it "shows multiple data and text sections" $
    showArm
      [ Section
          [ StringData ".L.str0" "looping...",
            StringData ".L.str1" "end of loop"
          ]
          ( Body
              "main"
              True
              [ Push (Regs [FP, LR]),
                Push (Regs [R8, R10, R12]),
                Mov (Reg FP) (Reg SP),
                Jmp ".L0",
                Define ".L1",
                Load (Reg R0) (Abs ".L.str0"),
                Jsr "_prints",
                Jsr "_println",
                Define ".L0",
                Mov (Reg R8) (Imm 0),
                Cmp (Reg R8) (Imm 1),
                Je ".L1",
                Load (Reg R0) (Abs ".L.str1"),
                Jsr "_prints",
                Jsr "_println",
                Mov (Reg R0) (Imm 0),
                Pop (Regs [R8, R10, R12]),
                Pop (Regs [FP, PC])
              ]
          ),
        Section
          [StringData ".L._prints_str0" "%.*s"]
          ( Body
              "_prints"
              False
              [ Push (Regs [LR]),
                Mov (Reg R2) (Reg R0),
                Load (Reg R1) (ImmOffset R0 (-4)),
                Load (Reg R0) (Abs ".L._prints_str0"),
                Jsr "printf",
                Mov (Reg R0) (Imm 0),
                Jsr "fflush",
                Pop (Regs [PC])
              ]
          ),
        Section
          [StringData ".L._println_str0" ""]
          ( Body
              "_println"
              False
              [ Push (Regs [LR]),
                Load (Reg R0) (Abs ".L._println_str0"),
                Jsr "puts",
                Mov (Reg R0) (Imm 0),
                Jsr "fflush",
                Pop (Regs [PC])
              ]
          )
      ]
      `shouldBe` T.unlines
        [ ".data",
          "\t.word 10",
          ".L.str0:",
          "\t.asciz \"looping...\"",
          "\t.word 11",
          ".L.str1:",
          "\t.asciz \"end of loop\"",
          ".text",
          ".global main",
          "main:",
          "\tpush {fp, lr}",
          "\tpush {r8, r10, r12}",
          "\tmov fp, sp",
          "\tb .L0",
          ".L1:",
          "\tldr r0, =.L.str0",
          "\tbl _prints",
          "\tbl _println",
          ".L0:",
          "\tmov r8, #0",
          "\tcmp r8, #1",
          "\tbeq .L1",
          "\tldr r0, =.L.str1",
          "\tbl _prints",
          "\tbl _println",
          "\tmov r0, #0",
          "\tpop {r8, r10, r12}",
          "\tpop {fp, pc}",
          "",
          ".data",
          "\t.word 4",
          ".L._prints_str0:",
          "\t.asciz \"%.*s\"",
          ".text",
          "_prints:",
          "\tpush {lr}",
          "\tmov r2, r0",
          "\tldr r1, [r0, #-4]",
          "\tldr r0, =.L._prints_str0",
          "\tbl printf",
          "\tmov r0, #0",
          "\tbl fflush",
          "\tpop {pc}",
          "",
          ".data",
          "\t.word 0",
          ".L._println_str0:",
          "\t.asciz \"\"",
          ".text",
          "_println:",
          "\tpush {lr}",
          "\tldr r0, =.L._println_str0",
          "\tbl puts",
          "\tmov r0, #0",
          "\tbl fflush",
          "\tpop {pc}"
        ]
