# ScmRb: Scheme Interpreter In Ruby 
# ScmRb is a Ruby implement of Lispy.
# (c) skandhas, 2012
# See also: Lispy(http://norvig.com/lispy.html)

module ScmRb
  SchemeSymbol = Symbol 

  class Env < Hash
    def initialize(parms = [], args = [], outer = nil)
      self.update Hash[parms.zip(args)]
      @outer = outer
    end  

    def find(v)
      self.key?(v) ? self : @outer.find(v)
    end
  end

  class << self
    def add_globals(env)
      [:exp, :log, :sin, :cos, :tan, :asin, :acos, :atan].each { |e| env[e] = Math.method e }
      [:+,:-,:*,:/,:<,:>,:<=,:>=,:equal?,:length].each { |e| env[e] = e.to_proc }
      env.update({
        eq?: :eql?.to_proc, append: :push.to_proc,
        cons:->(x,y){[x]+y }, car:->(x){x[0]}, cdr:->(x){x[1..-1]}, 
        list:->(*x){ x }, list?:->(x){ x.is_a? Array }, 
        null?:->(x){ x.empty? }, symbol?:->(x){ x.is_a? SchemeSymbol }
      })
    end

    $global_env = ScmRb.add_globals(Env.new)

    def evaluate(x, env = $global_env)
      if x.is_a? SchemeSymbol
        env.find(x)[x]
      elsif !(x.is_a? Array)
        x        
      else
        case x[0] 
        when :quote 
          _, exp = x
          exp
        when :if
          _, test, conseq, alt = x
          evaluate( evaluate(test, env)? conseq : alt , env)
        when :set!
          _, var, exp = x        
          env.find(var)[var] = evaluate(exp, env)
        when :define
          _, var, exp = x
          env[var] = evaluate(exp, env)
        when :lambda
          _, vars, exp = x
          lambda{ |*args| evaluate(exp, Env.new(vars, args, env)) } 
        when :begin
          x[1..-1].inject(nil){ |val, exp| val = evaluate(exp, env) }
        else  
          exps = x.map { |exp| evaluate(exp, env) }
          exps.shift.call(*exps)
        end
      end
    end

    def parse(s)
      read_from(tokenize(s))  
    end

    def tokenize(s)
      s.gsub(/(\(|\))/, ' \1 ').split
    end

    def read_from(tokens)
      raise ('unexpected EOF while reading') if tokens.empty?
      token = tokens.shift
      if token == '(' 
        l = []
        while tokens.first != ')' do l << read_from(tokens) end 
        tokens.shift
        l
      elsif token == ')'
        raise "unexpected \')\'"
      else
        atom(token)
      end 
    end

    def atom(token)
      case token
      when /-?\d*\.\d+/; token.to_f 
      when /-?\d+/;      token.to_i 
      else token.to_sym      
      end
    end

    def to_string(exp) 
      return "(#{exp.map{|e| to_string(e)}.join(' ')})" if exp.is_a? Array
      exp.is_a?(Proc)? '' : "#{exp}"
    end

    def repl(prompt = 'scm.rb> ')
      loop {(s = to_string(evaluate(parse(_raw_input(prompt))))).empty? ? nil: puts(s)}
    end
    def _raw_input(prompt = '') print(prompt); gets end
  end
end

if $0 == __FILE__
  ScmRb.repl
end
