# -*- mode: python -*-

def fn_library(name=None, srcs=[]):
  fail("Missing rule name.", attr="name", when=not name)
  fail("Needs exactly one source file.", attr="srcs", when=len(srcs) != 1)

  csrcs = ["%s.c" % src[:-3] for src in srcs]
  cname = "%s_c" % name
  native.genrule(
    name = cname,
    srcs = srcs,
    outs = csrcs,
    tools = ["//fntools:translate_scm"],
    cmd = "$(location //fntools:translate_scm) -o $(OUTS) $(SRCS)",
  )
  native.cc_library(
    name = name,
    srcs = csrcs,
    hdrs = ["%s.h" % src[:-3] for src in srcs],
  )

def fn_test(name=None, srcs=[], size=None):
  native.sh_test(
    name = name,
    size = size,
    srcs = ["//fntools:run_tests_sh"],
    data = ["//:fn", "//fntools:run_tests_fn"] + srcs,
  )
