subroutine sedadv(vvert,alphao1,alphao2,diffk1,diffk2,vvert1,vvert2,h1,h2,diffmo)

   implicit none
   
   real    :: zvvert2, zvvert1, vvert, vvert2, vvert1
   real    :: h2, h1, diffmo, diffk2, diffk1
   real    :: alphao2, alphao1

   zvvert1 = vvert * alphao1
   vvert1 = (vvert + zvvert1)/2.
   
   diffk1 = diffmo
   zvvert2 = zvvert1 * alphao2
   vvert2 = (zvvert1 + zvvert2)/2.
   
   diffk2 = diffmo
   diffk1 = diffk1 + vvert1 * h1
   diffk2 = diffk2 +vvert2 * h2
end subroutine sedadv