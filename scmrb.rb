# ScmRb: Scheme Interpreter In Ruby
# ScmRb is a Ruby implement of Lispy2.
# (c) skandhas, 2013
# version: 2.0.0
# See also: Lispy2(http://norvig.com/lispy2.html)

require 'stringio'

module ScmRb
  SchemeSymbol = Symbol

  # An environment: a Hash of {'var':val} pairs, with an outer Env.
  class Env < Hash
    # Bind parm list to corresponding args, or single parm to list of args.
    def initialize(parms = [], args = [], outer = nil)
      @outer = outer
      if parms.is_a? SchemeSymbol
        self.update({ parms => Array.new(args)})
      else
         parms.length == args.length  || raise("expected #{to_string(parms)}, given #{to_string(args)}")
         self.update Hash[parms.zip(args)]
      end
    end

    # Find the innermost Env where var appears.
    def find(v)
      self.key?(v) ? self : (@outer || raise("LookupError #{v}")) && @outer.find(v)
    end
  end

  # A user-defined Scheme procedure.
  class Procedure
    attr_accessor:parms, :exp, :env

    def initialize(parms, exp, env)
      @parms, @exp, @env = parms, exp, env
    end

    def call(*args)
      ScmRb.evaluate(exp, Env.new(@parms, args, @env))
    end
  end

  # An input port. Retains a line of chars."
  class InPort
    attr_accessor:file, :line

    @@tokenizer = /\s*(,@|[(\'`,)]|"(?:[\\].|[^\\"])*"|;.*|[^\s(\'"`,;)]*)(.*)/

    def initialize(file)
      @file, @line = file, ''
    end

    # Return the next token, reading new text into line buffer if needed.
    def next_token
      loop do
        @line = @file.gets if @line.empty?
        return $eof_object unless @line
        @@tokenizer =~ @line
        token, @line = $1, $2
        return token if token != '' and token[0] != ';'
      end
    end
  end

  $eof_object = :'#<eof-object>'
  $quotes = {"'" => :quote,   '`'  => :quasiquote,
             ',' => :unquote, ',@' => :'unquote-splicing'}

  class << self
    # Parse a program: read and expand/error-check it.
    def parse(inport)
      if inport.is_a? String
        inport = InPort.new(StringIO.new inport)
      end
      expand(read(inport), toplevel = true)
    end

    # Read the next character from an input port.
    def readchar(inport)
      unless inport.line.empty?
        ch, inport.line = inport.line[0], inport.line[1..-1]
        return ch
      end
      inport.file.read(1) || $eof_object
    end

    # Read a Scheme expression from an input port.
    def read(inport)
      read_ahead = ->(token) do
        if token == '('
          l = []
          while true
            token = inport.next_token
            token == ')' ? (return l) : (l << read_ahead.call(token))
          end
        elsif token == ')'
          raise 'Scheme-SyntaxError: unexpected `)'
        elsif $quotes.include? token
          [$quotes[token], read(inport)]
        elsif token == $eof_object
          raise 'Scheme-SyntaxError: unexpected EOF in list'
        else
          atom(token)
        end
      end

      token1 = inport.next_token
      token1 == $eof_object ?  $eof_object : read_ahead.call(token1)
    end

    # Numbers become numbers; #t and #f are booleans; "..." string; otherwise Symbol.
    def atom(token)
      case token
      when '#t'; true
      when '#f'; false
      when /^[+-]?\d*\.+[\d]*$/
        token.to_f
      when /^[+-]?\d+(?:\.\d)*?[Ee][+-]?\d{0,2}$/
        token.to_f
      when /^[+-]?\d+$/
        token.to_i
      else
        token[0] == '"' ? token[1..-2] : token.to_sym
      end
    end

    # Convert a Ruby object back into a Lisp-readable string.
    def to_string(x)
      case x
      when true;  '#t'
      when false; '#f'
      when Array; "(#{x.map{|e| to_string(e)}.join(' ')})"
      else
        x.to_s
      end
    end

    # Eval every expression from a file.
    def load(filename)
      repl(nil, InPort.new(open(filename)), nil)
    end

    # A prompt-read-eval-print loop.
    def repl(prompt = 'scmrb> ', inport = InPort.new(STDIN), out = STDOUT)
      STDERR.write "ScmRb version 2.0 \n"
      loop do
        begin
          STDERR.write(prompt) if prompt
          x = parse(inport)
          return if x == $eof_object
          val = evaluate(x)
          if not val.nil? and out
            out.puts to_string(val)
          end
        rescue => e
          puts "#{e.message}"
        end
      end
    end

    # Add some Scheme standard procedures.
    def add_globals(env)
      [:exp, :log, :sin, :cos, :tan, :asin, :acos, :atan, :sqrt].each do |e|
        env[e] = Math.method e
      end
      [:+,:-,:*,:/,:<,:>,:<=,:>=,:equal?,:length].each { |e| env[e] = e.to_proc }
      env.update({
        eq?: :eql?.to_proc, append: :+.to_proc ,
        not:->(x){not x}, :'=' => ->(x,y){x == y}, cons:->(x,y){[x]+y },
        car:->(x){x[0]}, cdr:->(x){x[1..-1]}, list:->(*x){ x },
        list?:->(x){ x.is_a? Array }, null?:->(x){ x.empty? },
        symbol?:->(x){ x.is_a? SchemeSymbol },
        boolean?:->(x){ x.is_a?(TrueClass) or x.is_a?(FalseClass)},
        pair?:->(x){ ScmRb.method(:pair?) }, apply:->(prc, l){ prc(*l) },
        display:->(x, port = STDOUT) do
          port.write(x.is_a?(String) ? x : to_string(x))
          nil
        end
      })
    end

    $global_env = ScmRb.add_globals(Env.new)

    # Evaluate an expression in an environment.
    def evaluate(x, env = $global_env)
      while true
        if x.is_a? SchemeSymbol
          return env.find(x)[x]
        elsif !(x.is_a? Array)
          return x
        else
          case x[0]
          when :quote
            _, exp = x
            return exp
          when :if
            _, test, conseq, alt = x
            x = evaluate(test, env)? conseq : alt
          when :set!
            _, var, exp = x
            env.find(var)[var] = evaluate(exp, env)
            return
          when :define
            _, var, exp = x
            env[var] = evaluate(exp, env)
            return
          when :lambda
            _, vars, exp = x
            return Procedure.new(vars, exp, env)
          when :begin
            x[1..-1].inject(nil){ |val, exp| val = evaluate(exp, env) }
            x = x.last
          else
            exps = x.map { |exp| evaluate(exp, env) }
            prc = exps.shift
            if prc.is_a? Procedure
              x = prc.exp
              env = Env.new(prc.parms, exps, prc.env)
            else
              return prc.call(*exps)
            end
          end
        end
      end
    end

    def expand(x, toplevel = false)
      guard(x, x != [])
      return x unless x.is_a? Array

      case x[0]
      when :quote
        guard(x, x.length == 2)
        return x
      when :if
        x << nil if x.length == 3
        guard(x, x.length == 4)
        return x.map{|e| expand(e)}
      when :set!
        guard(x, x.length == 3)
        var = x[1]
        guard(x, var.is_a?(SchemeSymbol))
        return [:set!, var, expand(x[2])]
      when :define, :'define-macro'
        guard(x, x.length >= 3)
        _def, v, body = x[0], x[1], x[2..-1]
        if  v.is_a? Array and v
          f, args = v[0], v[1..-1]
          return expand([_def, f, [:lambda, args] + body])
        else
          guard(x, x.length == 3) && guard(x, v.is_a?(SchemeSymbol))
          exp = expand(x[2])
          if _def == :'define-macro'
            guard(x, toplevel, 'define-macro only allowed at top level')
            prc = evaluate(exp)
            guard(x, prc.respond_to?(:call))
            $macro_table[v] = prc
            return
          end
          return [:define, v, exp]
        end
      when :begin
        return if x.length == 1
        return x.map{|xi| expand(xi, toplevel)}
      when :lambda
        guard(x, x.length >= 3)
        vars, body = x[1], x[2..-1]

        guard(x,
             (vars.is_a? Array and vars.all?{|v| v.is_a? SchemeSymbol }) ||
              vars.is_a?(SchemeSymbol),
              'illegal lambda argument list')

        body.length == 1?  exp = body[0] : exp = [:begin] + body
        return [:lambda, vars, expand(exp)]
      when :quasiquote
        guard(x, x.length == 2)
        return expand_quasiquote(x[1])
      end

      if x[0].is_a? SchemeSymbol and $macro_table.include? x[0]
        expand($macro_table[x[0]].call(*x[1..-1]), toplevel)
      else
        x.map{|xi|  expand xi}
      end
    end

    # Signal a syntax error if predicate is false.
    def guard(x, predicate, msg = 'wrong length')
      raise "Scheme-SyntaxError: #{x} : #{msg}" unless predicate
    end

    def pair?(x) x != [] && x.is_a?(Array) end
    def cons(x, y) [x] + y end

    # Expand `x => 'x; `,x => x; `(,@x y) => (append x y)
    def expand_quasiquote(x)
      return [:quote, x] unless pair? x
      guard(x, x[0] != :'unquote-splicing', "can't splice here" )
      if x[0] == :unquote
        guard(x, x.length == 2)
        x[1]
      elsif pair?(x[0]) && x[0][0] == :'unquote-splicing'
        guard(x[0], x[0].length == 2)
        [:append, x[0][1], expand_quasiquote(x[1..-1])]
      else
        [:cons, expand_quasiquote(x[0]), expand_quasiquote(x[1..-1])]
      end
    end

    def let(*args)
      x = cons(:let, args)
      guard(x, args.length > 1)
      bindings, body = args[0], args[1..-1]
      guard(x,
            bindings.all? do |b|
              b.is_a?(Array) and b.length == 2 and b[0].is_a?(SchemeSymbol)
            end,
            'illegal binding list')
      vars, vals = bindings.shift.zip(*bindings)
      [[:lambda, Array.new(vars)] + body.map{|e| expand(e)}] + vals.map{|e| expand(e)}
    end
  end
  $macro_table = {let:ScmRb.method(:let)} ## More macros can go here
end

if $0 == __FILE__
  ScmRb.repl
end
