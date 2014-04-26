
RESULT = "ando"
RESULT << ".exe" if Rake.application.windows?
IMP = FileList["import/**/*.d"]
SRC = ["main.d"]

file RESULT => SRC + IMP do |t|
  sh "dmd", "-I./import", "-of#{t.name}", *IMP, *SRC
end

task :unittest => SRC + IMP do |t|
  sh "dmd", "-unittest", "-I./import", "-of#{t.name}", *IMP, "-run", *SRC
end

task default: 'unittest'

