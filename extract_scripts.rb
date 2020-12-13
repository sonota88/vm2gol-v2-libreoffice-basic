require "fileutils"
require "rexml/document"

def xpath_match(el, xpath)
  return REXML::XPath.match(el, xpath)
end

file = ARGV[0]
bname = File.basename(file, ".fods")

xml = File.read(file)
doc = REXML::Document.new(xml)

lib_els = xpath_match(doc, "//ooo:library-embedded")

lib_els.each do |lib_el|
  lib_name = lib_el["ooo:name"]

  mod_els = xpath_match(lib_el, "ooo:module")
  mod_els.each do |mod_el|
    mod_name = mod_el["ooo:name"]

    dir = [bname, lib_name].join("/")
    FileUtils.mkdir_p dir

    path = File.join(dir, mod_name + ".libo.bas")
    src = xpath_match(mod_el, "ooo:source-code")[0].text
    File.open(path, "wb") do |f|
      f.puts "rem -*- mode: basic -*-"
      f.puts ""
      f.puts src.strip
    end
  end
end
