#!/usr/bin/ruby

if ARGV.empty?
  puts 'Usage: newmodule.rb MODULENAME'
  exit(0)
end

modname = ARGV[0]

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

files << ["#{modname}.c", newline + mkinclude(modname)]
files << ["#{modname}.h", guard(newline + mkinclude('value'),
                                "_#{modname.upcase}_H_")]
files << ["#{modname}-test.h",
          guard(newline + "extern" +
                newline + "void #{modname}_tests();" +
                newline,
                "_#{modname.upcase}_TEST_H_")]
files << ["#{modname}-test.c",
          newline + mkinclude("tests") +
          newline + mkinclude(modname) +
          newline + "TEST(#{modname}_simple) {" +
          newline + "  // TODO" +
          newline + "}" +
          newline +
          newline + "extern" +
          newline + "void #{modname}_tests() {" +
          newline + "  TESTRUN(#{modname}_simple);" +
          newline + "}" +
          newline]

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
