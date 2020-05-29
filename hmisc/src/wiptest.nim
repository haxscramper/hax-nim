import hmisc/cl_loop

echo loop((
  lfor i in 0..2; lfor q in @[1,2,4];
  lcollect i; lcollect "i * q"))
