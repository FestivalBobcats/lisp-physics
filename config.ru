ENV['RACK_ENV'] ||= 'production'
require './app'
map '/' do
  run ViralThoughts
end