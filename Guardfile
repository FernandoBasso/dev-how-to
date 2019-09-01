require 'asciidoctor'

guard 'shell' do
  watch(/^*\.adoc$/) {|m|
    Asciidoctor.convert_file(m[0], :safe => :unsafe, :to_file => "#{m[0]}.conv.html")
  }
end
