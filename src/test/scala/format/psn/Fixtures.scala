package shogi
package format.psn

// format: off
object Fixtures {

  val simple = "P-7f P-8d G-7h P-8e"

  val raws = List(
    "P2g-2f P3c-3d P2f-2e B2b-3c P7g-7f P4c-4d S3i-4h P8c-8d G6i-7h S7a-6b S7i-6h G6a-5b P4g-4f S3a-2b G4i-5h G4a-3b S4h-4g G5b-4c K5i-6i K5a-4a P1g-1f B3c-4b S4g-5f S2b-3c P3g-3f P5c-5d N2i-3g B4b-6d R2h-4h P7c-7d S5f-6e B6d-4b S6ex7d P8d-8e B8h-7g K4a-3a K6i-7i K3a-2b B7g-9e P8e-8f P8gx8f B4bx8f P'8c B8fx6h+ K7ix6h R8b-7b P7f-7e P9c-9d B9e-8f P3d-3e G5h-4g P3ex3f G4gx3f N8a-9c P'3d S3c-4b P4f-4e S'8e P4ex4d S8ex8f P4dx4c+ S4bx4c B'6a P'7g G7h-7i P'4g R4hx4g P'4d B6ax7b+ B'2g R4g-4f S8f-8g+ +B7bx6b B2g-3h+ S'4a G3b-4b +B6b-5a G4bx4a R'7b G4a-3b G'3c N2ax3c +B5ax3c K2b-2a R7bx3b+",
    "P7g-7f P3c-3d B8hx2b+ S3ax2b B'4e B'8e R2h-7h S2b-3c K5i-4h P4c-4d B4e-5f P6c-6d K4h-3h S7a-7b P7f-7e K5a-4b K3h-2h P5c-5d P8g-8f B8e-9d P7e-7d P5d-5e P7dx7c+ S7bx7c B5f-7d S7cx7d R7hx7d B9dx6g+ G6i-5h +B6gx8i P'7c +B8i-4e R7dx6d R8b-6b R6dx6b+ G6ax6b R'8b P'6a S3i-3h R'6i S7i-6h R6ix9i+ R8bx8c+ L'2d S'3f N'1e S6h-5i +B4e-6c P7c-7b+ +B6cx7b +R8cx7b G6bx7b P1g-1f P3d-3e"
  )

  val europeHoskingPotter = """[Name "Thomas Majewski"]
[Email ""]
[Country "Belgium"]
[Sente "Hosking Tony"]
[Gote "Pottier Frederic"]
[SenteGrade "3dan"]
[GoteGrade "2dan"]
[Result "1-0"]
[Comment ""]
[Source "Tony's gamescore"]
[Event "European Championships 1997"]
[Date "19970822"]
[UTCDate "1997.08.22"]
[Round "3"]
[Venue "RIKAB"]
[Proam "Amateur"]
P2g-2f P3c-3d P2f-2e B2b-3c P7g-7f P4c-4d S3i-4h P8c-8d G6i-7h S7a-6b S7i-6h G6a-5b P4g-4f S3a-2b G4i-5h G4a-3b S4h-4g G5b-4c K5i-6i K5a-4a P1g-1f B3c-4b S4g-5f S2b-3c P3g-3f P5c-5d N2i-3g B4b-6d R2h-4h P7c-7d S5f-6e B6d-4b S6ex7d P8d-8e B8h-7g K4a-3a K6i-7i K3a-2b B7g-9e P8e-8f P8gx8f B4bx8f P'8c B8fx6h+ K7ix6h R8b-7b P7f-7e P9c-9d B9e-8f P3d-3e G5h-4g P3ex3f G4gx3f N8a-9c P'3d S3c-4b P4f-4e S'8e P4ex4d S8ex8f P4dx4c+ S4bx4c B'6a P'7g G7h-7i P'4g R4hx4g P'4d B6ax7b+ B'2g R4g-4f S8f-8g+ +B7bx6b B2g-3h+ S'4a G3b-4b +B6b-5a G4bx4a R'7b G4a-3b G'3c N2ax3c +B5ax3c K2b-2a R7bx3b+
"""

  val europeHoskingLamb = """
[Name "Thomas Majewski"]
[Email ""]
[Country "Belgium"]
[Sente "Hosking Tony"]
[Gote "Lamb Stephen"]
[SenteGrade "3dan"]
[GoteGrade "4dan"]
[Result "0-1"]
[Comment ""]
[Source "Tony's gamescore"]
[Event "European Championships 1997"]
[Date "19970822"]
[UTCDate "1997.08.22"]
[Round "1"]
[Venue "RIKAB"]
[Proam "Amateur"]
P7g-7f P3c-3d B8hx2b+ S3ax2b B'4e B'8e R2h-7h S2b-3c K5i-4h P4c-4d B4e-5f P6c-6d K4h-3h S7a-7b P7f-7e K5a-4b K3h-2h P5c-5d P8g-8f B8e-9d P7e-7d P5d-5e P7dx7c+ S7bx7c B5f-7d S7cx7d R7hx7d B9dx6g+ G6i-5h +B6gx8i P'7c +B8i-4e R7dx6d R8b-6b R6dx6b+ G6ax6b R'8b P'6a S3i-3h R'6i S7i-6h R6ix9i+ R8bx8c+ L'2d S'3f N'1e S6h-5i +B4e-6c P7c-7b+ +B6cx7b +R8cx7b G6bx7b P1g-1f P3d-3e
"""

  val noTagButResult = "1. P2g-2f 2. P3c-3d 3. P2f-2e 4. B2b-3c 5. P7g-7f 6. P4c-4d 7. S3i-4h 8. P8c-8d 9. G6i-7h 10. S7a-6b 11. S7i-6h 12. G6a-5b 13. P4g-4f 14. S3a-2b 15. G4i-5h 16. G4a-3b 17. S4h-4g 18. G5b-4c 19. K5i-6i 20. K5a-4a 21. P1g-1f 22. B3c-4b 23. S4g-5f 24. S2b-3c 25. P3g-3f 26. P5c-5d 27. N2i-3g 28. B4b-6d 29. R2h-4h 30. P7c-7d 31. S5f-6e 32. B6d-4b 33. S6ex7d 34. P8d-8e 35. B8h-7g 36. K4a-3a 37. K6i-7i 38. K3a-2b 39. B7g-9e 40. P8e-8f 41. P8gx8f 42. B4bx8f 43. P@8c 44. B8fx6h+ 45. K7ix6h 46. R8b-7b 47. P7f-7e 48. P9c-9d 49. B9e-8f 50. P3d-3e 51. G5h-4g 52. P3ex3f 53. G4gx3f 54. N8a-9c 55. P@3d 56. S3c-4b 57. P4f-4e 58. S@8e 59. P4ex4d 60. S8ex8f 61. P4dx4c+ 52. S4bx4c 63. B@6a 64. P@7g 65. G7h-7i 66. P@4g 67. R4hx4g 68. P@4d 69. B6ax7b+ 70. B@2g 71. R4g-4f 72. S8f-8g+ 73. +B7bx6b 74. B2g-3h+ 75. S@4a 76. G3b-4b 77. +B6b-5a 78. G4bx4a 79. R@7b 80. G4a-3b 81. G@3c 82. N2ax3c 83. +B5ax3c 84. K2b-2a 85. R7bx3b+ 1-0"

  val inlineTags = """
[Name "Thomas Majewski"]
[Email ""]
[Country "Belgium"]
[Sente "Miyamoto Toyokazu"]
[Gote "Fukumura Tsutomu"]
[SenteGrade "4dan"]
[GoteGrade "5dan"]
[Result "1-0"]
[Comment ""]
[Source "Mr. Miyamoto's and Mr. Fukumura's gamescore"]
[Event "European Championships 1997"]
[Date "19970823"]
[UTCDate "1997.08.23"]
[Round "4"]
[Venue "RIKAB"]
[Proam "Amateur"]
P7g-7f P3c-3d P2g-2f P4c-4d S3i-4h S3a-4b P2f-2e S4b-3c P5g-5f P5c-5d G6i-7h G4a-3b S7i-6h S7a-6b K5i-6i K5a-4a P3g-3f B2b-3a S4h-3g G6a-5b S3g-4f G5b-4c P5f-5e P5dx5e P3f-3e P3dx3e S4fx3e P'3d P2e-2d P2cx2d S3ex2d S3cx2d R2hx2d P'2c R2d-2h S6b-5c N2i-3g S5c-5d N3g-2e S'2d P9g-9f P5e-5f K6i-7i B3a-6d P'5e S5d-4e S'6e B6d-3a S6e-5d S4ex5d P5ex5d G4cx5d P'3c N2ax3c N2ex3c+ G3bx3c N'4f G5d-4e S'5d G4e-3f G4i-5h N'5a B8h-9g S'4b S5d-6e R8b-5b S6e-5d P5f-5g+ G5hx5g P'5c S5d-6e P4d-4e S6e-5f P4ex4f G5gx4f G3f-3g R2h-2i P1c-1d B9g-8h P'4d P'3e S4b-4c P3ex3d S4cx3d S5f-5e N'3b R2i-5i G3c-4c B8h-9g S2d-3c N8i-7g P4d-4e P'3e P4ex4f P3ex3d S3cx3d P'5d P5cx5d B9gx3a+ K4ax3a B'6a R5b-8b B6ax4c+ N5ax4c S5ex5d P'5h R5i-3i P'3h R3i-2i P3h-3i+ R2ix3i B'4h R3i-4i G3g-3h R4ix4h G3hx4h G'3c B'2d B'5c K3a-2a S5dx4c+ S3dx4c S'2b K2a-1b B5c-3a+ B2dx3c +B3a-2a
"""

  val inlineComments = """[Name "Eric Cheymol"]
[Email ""]
[Country "Belgium"]
[Sente "Boekschoten Michiel"]
[Gote "Lamb Stephen"]
[SenteGrade "3dan"]
[GoteGrade "4dan"]
[Result "0-1"]
[Comment ""]
[Source ""]
[Event "European Championships 1997"]
[Date "19970822"]
[UTCDate "1997.08.22"]
[Round "1"]
[Venue "Brussels"]
[Proam "Amateur"]
P7g-7f P3c-3d P2g-2f P4c-4d S3i-4h S3a-3b P5g-5f R8b-4b G4i-5h K5a-6b K5i-6h S7a-7b K6h-7h G4a-5b P2f-2e B2b-3c B8h-7g S3b-4c P3g-3f K6b-7a B7g-6h P4d-4e S7i-8h K7a-8b P1g-1f P6c-6d S4h-3g P1c-1d P2e-2d P2cx2d B6hx2d R4b-2b P'2e B3c-4d B2d-6h N2a-3c S3g-2f R2b-2a P3f-3e P3dx3e {Perhaps P-5d was better. (Steve)} S2fx3e B4dx3e B6hx3e R2ax2e R2hx2e N3cx2e R'3a R'3i B3ex5c+ {This trick doesn't work. See the next move...~(Steve)} R3ix6i+ {This gets me a bishop and gold for the rook, ~though Michiel does manage to get some ~counterplay. (Steve)} K7hx6i G5bx5c R'2a S4c-5b {A faster way to win is to forget about defending ~and instead attack with P*5g! Then:~(i) Rx6a+, Px5h+, Kx5h, Sx6a, Rx6a+, R*3h mates~(ii) Gx5g, S*4h, Rx6a+, Sx6a, Rx6a+, R*6h mates~(iii) Gx5g, S*4h, G-5h, G*5g, Rx6a+, Sx6a, Rx6a+, ~Gx5h mates~(iv) Gx5g, S*4h, G-5h, G*5g, Gx4h, Gx4h, Rx6a+, ~Sx6a, Rx6a+, R*5i mates~(v) Gx5g, S*4h, G-5h, G*5g, Gx5g, Sx5g+.~No way I'd have seen all this in the game ~though :-) (Steve)} B'6b P'5a B6bx5a+ G6ax5a R3ax5a+ G'6a G'6b G6ax5a R2ax5a+ R'3i K6i-7h {Not good, but Michiel cannot play:~(i) K-6h since B*2d~(ii) G-5i, B*5h, K-7h (not Kx5h, S*5g mates), ~S*6i and I either mate or fork rook and king ~with a bishop drop on 2d or 3c next.~~Michiel should probably have played G*5i, then I ~would have had to play S*6a which defends ~adequately. (Steve)} B'6i K7h-6h B6ix5h+ K6hx5h R3i-3h+ {This mates. (Steve)} B'4h B'4i K5h-6h +R3hx4h K6h-7g B'6h"""

  val withNag = """
[Email ""]
[Country "Poland"]
[Sente "Karolina"]
[Gote "Wojtek"]
[SenteGrade "30kyu"]
[GoteGrade "30kyu"]
[Result "1-0"]
[Comment ""]
[Source "Shogi Harbour"]
[Event "Road to Shodan Ep. 4"]
[Date "?"]
[Round "?"]
[Venue "?"]
[Proam "Amateur"]
[Annotator "Sad Penguin and Emo Bear"]

1. P-7f 2. P-3d 3. P-7e 4. P-8d 5. R-7h 6. P-8e 7. K-4h 8. P-8f $2 9. Px8f 10. Rx8f 11. P-7d 12. Px7d 13. Bx2b+ 14. Sx2b 15. B*9e $18 1-0
"""

  val commentsAndVariations = """
[Name "Eric Cheymol"]
[Email ""]
[Country "Belgium"]
[Sente "Boekschoten Michiel"]
[Gote "Lamb Stephen"]
[SenteGrade "3dan"]
[GoteGrade "4dan"]
[Result "0-1"]
[Comment ""]
[Source ""]
[Event "European Championships 1997"]
[Date "19970822"]
[UTCDate "1997.08.22"]
[Round "1"]
[Venue "Brussels"]
[Proam "Amateur"]
P7g-7f P3c-3d P2g-2f P4c-4d S3i-4h S3a-3b P5g-5f R8b-4b G4i-5h K5a-6b K5i-6h S7a-7b K6h-7h G4a-5b P2f-2e B2b-3c B8h-7g S3b-4c P3g-3f K6b-7a B7g-6h P4d-4e S7i-8h K7a-8b P1g-1f P6c-6d S4h-3g P1c-1d P2e-2d P2cx2d B6hx2d R4b-2b P'2e B3c-4d B2d-6h N2a-3c S3g-2f R2b-2a P3f-3e P3dx3e {Perhaps P-5d was better. (Steve)} S2fx3e B4dx3e B6hx3e R2ax2e R2hx2e N3cx2e R'3a R'3i B3ex5c+ {This trick doesn't work. See the next move...~(Steve)} R3ix6i+ {This gets me a bishop and gold for the rook, ~though Michiel does manage to get some ~counterplay. (Steve)} K7hx6i G5bx5c R'2a S4c-5b {A faster way to win is to forget about defending ~and instead attack with P*5g! Then:~(i) Rx6a+, Px5h+, Kx5h, Sx6a, Rx6a+, R*3h mates~(ii) Gx5g, S*4h, Rx6a+, Sx6a, Rx6a+, R*6h mates~(iii) Gx5g, S*4h, G-5h, G*5g, Rx6a+, Sx6a, Rx6a+, ~Gx5h mates~(iv) Gx5g, S*4h, G-5h, G*5g, Gx4h, Gx4h, Rx6a+, ~Sx6a, Rx6a+, R*5i mates~(v) Gx5g, S*4h, G-5h, G*5g, Gx5g, Sx5g+.~No way I'd have seen all this in the game ~though :-) (Steve)} B'6b P'5a B6bx5a+ G6ax5a R3ax5a+ G'6a G'6b G6ax5a R2ax5a+ R'3i K6i-7h {Not good, but Michiel cannot play:~(i) K-6h since B*2d~(ii) G-5i, B*5h, K-7h (not Kx5h, S*5g mates), ~S*6i and I either mate or fork rook and king ~with a bishop drop on 2d or 3c next.~~Michiel should probably have played G*5i, then I ~would have had to play S*6a which defends ~adequately. (Steve)} B'6i K7h-6h B6ix5h+ K6hx5h R3i-3h+ {This mates. (Steve)} B'4h B'4i K5h-6h +R3hx4h K6h-7g B'6h
"""

  val variations = """
P7g-7f P3c-3d P2g-2f P4c-4d S3i-4h S3a-3b P5g-5f R8b-4b G4i-5h K5a-6b K5i-6h S7a-7b K6h-7h G4a-5b P2f-2e B2b-3c B8h-7g S3b-4c P3g-3f K6b-7a B7g-6h P4d-4e S7i-8h K7a-8b P1g-1f P6c-6d S4h-3g P1c-1d P2e-2d P2cx2d B6hx2d R4b-2b P'2e B3c-4d B2d-6h N2a-3c S3g-2f R2b-2a P3f-3e P3dx3e {P-5d} S2fx3e B4dx3e B6hx3e R2ax2e R2hx2e N3cx2e R'3a R'3i B3ex5c+ R3ix6i+ K7hx6i G5bx5c R'2a S4c-5b {P*5g!} B'6b P'5a B6bx5a+ G6ax5a R3ax5a+ G'6a G'6b G6ax5a R2ax5a+ R'3i K6i-7h {K-6h B*2d} B'6i K7h-6h B6ix5h+ K6hx5h R3i-3h+ B'4h B'4i K5h-6h +R3hx4h K6h-7g B'6h
"""

  val gamesForPerfTest = List(europeHoskingPotter, europeHoskingLamb,
    noTagButResult, inlineTags, inlineComments, withNag,
    commentsAndVariations, variations)
}
