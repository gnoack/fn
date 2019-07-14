#!/usr/bin/ruby

if ARGV.empty?
  puts 'Usage: newmodule.rb [--lisp] MODULENAME'
  exit(0)
end

modname = ARGV[0]
lisp_mode = false

if modname == '--lisp'
  modname = ARGV[1]
  lisp_mode = true
end

files = []

newline = "\n"

def mkinclude(name)
  "#include \"#{name}.h\"\n"
end

def guard(file, guarddef)
  return "#ifndef #{guarddef}\n" +
    file + "\n" +
    "#define #{guarddef} 0\n" +
    "#endif  // #{guarddef}\n"
end

files << ["#{modname}-test.h",
          guard(newline + "void #{modname}_tests();" +
                newline,
                "_#{modname.upcase}_TEST_H_")]

if lisp_mode
  empty_lisp_file = newline + "(" +
    newline + ")" +
    newline

  files << ["#{modname}.h",
            guard(newline + mkinclude("value") +
                  newline + "oop #{modname}_decls();" +
                  newline,
                  "_#{modname.upcase}_H_")]
  files << ["#{modname}.fn", empty_lisp_file]
  files << ["#{modname}-test.fn", empty_lisp_file]
else
  files << ["#{modname}.h", guard(newline + mkinclude('value'),
                                  "_#{modname.upcase}_H_")]
  files << ["#{modname}.c", newline + mkinclude(modname)]
  files << ["#{modname}-test.c",
            newline + mkinclude("tests") +
            newline + mkinclude(modname) +
            newline + "TEST(#{modname}_simple) {" +
            newline + "  // TODO" +
            newline + "}" +
            newline +
            newline + "void #{modname}_tests() {" +
            newline + "  TESTRUN(#{modname}_simple);" +
            newline + "}" +
            newline]
end

files.each do |f|
  filename, contents = f
  puts "*** New file #{filename}:"
  print contents
end

files.each do |f|
  filename, contents = f
  file = open(filename, 'w')
  file << contents
  file.close
end

puts "Remaining steps:"
puts "* Now, in Makefile, add #{modname}-test.o, #{modname}.o."
puts "* Also include #{modname}-test.h from tests.c, call"
puts "  #{modname}_tests() from there!"
if lisp_mode
  puts "* Call load_decls(#{modname}_decls()) in init() in tests.c"
  puts "  and include #{modname}.h there, too."
end
