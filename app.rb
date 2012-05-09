require 'bundler/setup'
require 'sinatra/base'

class ViralThoughts < Sinatra::Base
  configure do
    set :root, File.dirname(__FILE__)
  end

  get '/' do
    File.read(File.join(settings.root, 'views', 'index.html'))
  end

  get '/hatch' do
    hatchery_path = File.join(settings.root, 'lib', 'hatchery', 'lisp-universe')
    `#{hatchery_path} #{params[:frames]} #{params[:width]} #{params[:height]} #{params[:particle_count]}`
  end

end