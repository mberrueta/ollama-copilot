module Greetings
  class HelloWorld
    def initialize(name)
      @name = name
    end

    def say_hello
      puts "Hello, #{@name}!"
    end

    def say_goodbye
      puts "Goodbye, #{@name}!"
    end

    def greet_in_language(language)
      case language.downcase
      when 'spanish'
        puts "¡Hola, #{@name}!"
      when 'french'
        puts "Bonjour, #{@name}!"
      when 'german'
        puts "Hallo, #{@name}!"
      when 'italian'
        puts "Ciao, #{@name}!"
      when 'portuguese'
        puts "OlÃ¡, #{@name}!"
      else
        puts "Hello, #{@name}!"
      end
    end
  end

  def get_current_date
    date = DateTime.now.strftime("%Y-%m-%d %H:%M:%S")
    puts "Current date and time: #{date}"
  end

  # Call Open AI api to ask translation
  def call(to_language)


  end
end
