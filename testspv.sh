echo "=======VALiDATION=ERROR========"
spirv-val out.spv
echo "=========DISASEMBLY==========="
spirv-dis out.spv -o out.spvasm
cat out.spvasm

  
