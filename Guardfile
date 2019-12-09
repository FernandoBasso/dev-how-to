# vim: set filetype=ruby:

require 'asciidoctor'

guard 'shell' do
  watch(/.*.adoc$/) { |m|
    Asciidoctor.convert_file(
      m[0],
      :safe => :unsafe,
      :to_file => "#{m[0]}.html",
      header_footer: true,
      :attributes => {
        'favicon' => 'https://fernandobasso.dev/cmdline.png',
        # linkcss works for asciidoctor.css and pygments-default.css
        # but not for the matjax stuff.
        'linkcss!' => '',
        'toc' => 'left',
        'webfonts!' => '',
        'icons!' => 'font',
        'sectnums!' => '',
        'sectlinks' => '',
        'source-highlighter' => 'pygments',
        'pygments-css' => 'class',
      }
    )
  }
end
