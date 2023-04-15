data ← •Import ⊑•args
Chooses ← 23-˜'A'-˜1⊸⊑¨
ChoosesPoints ← +´1+Chooses
Opponent ← 'A'-˜⊑¨
outcomes ← [3‿6‿0, 0‿3‿6, 6‿0‿3]
opponentPoints ← +´(⊑⟜outcomes)¨ (Opponent ⋈¨ Chooses) data
Points ← ChoosesPoints⊸+⟜opponentPoints
solution1 ← Points data
•Show solution1
