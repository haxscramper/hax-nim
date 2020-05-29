import hmisc/cl_loop

loop((
  lfor i in 0..2; lfor q in @[1,2,4];
  lcollect i; collect i * 2))
