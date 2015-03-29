# -*- mode:python -*-

load("//fntools/build_rules/fn", "fn_library", "fn_test")

# The whole thing.

cc_library(
    name = "runtime",
    srcs = ["runtime.c"],
    deps = [
        ":arrays",
        ":byte-buffer",
        ":compiler",
        ":cons",
        ":continuations",
        ":data",
        ":debug",
        ":deserialize",
        ":dl",
        ":eval",
        ":gc",
        ":interpreter",
        ":lang",
        ":macros",
        ":memory",
        ":modules",
        ":native-compiler",
        ":objects",
        ":parser",
        ":pegs-parser",
        ":pegs",
        ":pprint",
        ":primitives",
        ":procedures",
        ":serialize",
        ":strings",
        ":symbols",
        ":utils",
        ":value",
        ":vars",
    ],
    # TODO: Trim down the public interface.
    hdrs = [
        "cons.h",
        "deserialize.h",
        "eval.h",
        "gc.h",
        "interpreter.h",
        "procedures.h",
        "runtime.h",
        "strings.h",
        "value.h",
    ],
    visibility = ["//visibility:public"],
)

# Fine-grained libraries.

fn_library(
  name = "arrays",
  srcs = ["arrays.fn"],
)

fn_library(
  name = "byte-buffer",
  srcs = ["byte-buffer.fn"],
)

fn_library(
  name = "compiler",
  srcs = ["compiler.fn"],
)

cc_library(
  name = "cons",
  srcs = ["cons.c"],
)

fn_library(
  name = "continuations",
  srcs = ["continuations.fn"],
)

cc_library(
  name = "data",
  srcs = ["data.c"],
)

cc_library(
  name = "debug",
  srcs = ["debug.c"],
  deps = [":data"],
)

cc_library(
  name = "deserialize",
  srcs = ["deserialize.c"],
)

cc_library(
  name = "dl",
  srcs = ["dl.c"],
  linkopts = ["-ldl"],
)

cc_library(
  name = "eval",
  srcs = ["eval.c"],
)

cc_library(
  name = "gc",
  srcs = ["gc.c"],
)

cc_library(
  name = "interpreter",
  srcs = ["interpreter.c"],
)

fn_library(
  name = "lang",
  srcs = ["lang.fn"],
)

fn_library(
  name = "macros",
  srcs = ["macros.fn"],
)

cc_library(
  name = "memory",
  srcs = ["memory.c"],
)

fn_library(
  name = "modules",
  srcs = ["modules.fn"],
)

cc_library(
  name = "native-compiler",
  srcs = ["native-compiler.c"],
)

fn_library(
  name = "objects",
  srcs = ["objects.fn"]
)

fn_library(
  name = "parser",
  srcs = ["parser.fn"]
)

fn_library(
  name = "pegs-parser",
  srcs = ["pegs-parser.fn"]
)

fn_library(
  name = "pegs",
  srcs = ["pegs.fn"]
)

fn_library(
  name = "pprint",
  srcs = ["pprint.fn"]
)

cc_library(
  name = "primitives",
  srcs = ["primitives.c"],
)

cc_library(
  name = "procedures",
  srcs = ["procedures.c"],
)

fn_library(
  name = "serialize",
  srcs = ["serialize.fn"],
)

cc_library(
  name = "strings",
  srcs = ["strings.c"],
)

cc_library(
  name = "symbols",
  srcs = ["symbols.c"],
)

cc_test(
  name = "tests",
  srcs = [
      "tests.c",
      "cons-test.c",
      "interpreter-test.c",
      "memory-test.c",
      "strings-test.c",
      "value-test.c",
  ],
  deps = [":runtime"],
)

fn_library(
  name = "utils",
  srcs = ["utils.fn"],
)

cc_library(
  name = "value",
  srcs = ["value.c"],
)

cc_library(
  name = "vars",
  srcs = ["vars.c"],
)


# Tests

fn_test(
  name = "arrays_test",
  srcs = ["arrays-test.fn"],
)

fn_test(
  name = "byte_buffer_test",
  srcs = ["byte-buffer-test.fn"],
)

fn_test(
  name = "compiler_test",
  srcs = ["compiler-test.fn"],
)

fn_test(
  name = "continuations_test",
  srcs = ["continuations-test.fn"],
)

fn_test(
  name = "data_test",
  srcs = ["data-test.fn"],
)

fn_test(
  name = "dl_test",
  srcs = ["dl-test.fn"],
)

fn_test(
  name = "lang_test",
  srcs = ["lang-test.fn"],
)

fn_test(
  name = "macros_test",
  srcs = ["macros-test.fn"],
)

fn_test(
  name = "modules_test",
  srcs = ["modules-test.fn"],
)

fn_test(
  name = "objects_test",
  srcs = ["objects-test.fn"],
)

fn_test(
  name = "parser_test",
  srcs = ["parser-test.fn"],
)

fn_test(
  name = "pegs_parser_test",
  srcs = ["pegs-parser-test.fn"],
)

fn_test(
  name = "pegs_test",
  srcs = ["pegs-test.fn"],
)

fn_test(
  name = "pprint_test",
  srcs = ["pprint-test.fn"],
)

fn_test(
  name = "primitives_test",
  srcs = ["primitives-test.fn"],
)

fn_test(
  name = "serialize_test",
  srcs = ["serialize-test.fn"],
)

fn_test(
  name = "utils_test",
  srcs = ["utils-test.fn"],
)
