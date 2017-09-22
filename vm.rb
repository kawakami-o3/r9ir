#!/usr/bin/env ruby
# coding: utf-8
#
#

require 'parslet'

=begin
ジャンプ命令に対応するため、一命令づつ食わせる方式は修正する必要がある。
パーサーを拡張すれば、一行一行パースするのではなく、一括でパースできるようになるはず。
=end

class AsmParser < Parslet::Parser
	rule(:lparen) { str('(') >> space? }
	rule(:rparen) { str(')') >> space? }
	rule(:comma)  { str(',') >> space? }

	rule(:space) {match('\s').repeat(1) }
	rule(:space?) { space.maybe }

	rule(:register) {
	 	str('rax') | str('eax') | str('ax') |
	 	str('rbx') | str('ebx') | str('bx') |
	 	str('rcx') | str('ecx') | str('cx') |
	 	str('rdx') | str('edx') | str('dx') |
	 	str('rsp') | str('esp') | str('sp') |
	 	str('rbp') | str('ebp') | str('bp') |
	 	str('rsi') | str('esi') | str('si') |
	 	str('rdi') | str('edi') | str('di') |
	 	str('rip') | str('eip') | str('ip') |
	 	str('r8d') | str('r8') |
	 	str('r9d') | str('r9') |
	 	str('r10d') | str('r10') |
	 	str('r11d') | str('r11') |
	 	str('r12d') | str('r12') |
	 	str('r13d') | str('r13') |
	 	str('r14d') | str('r14') |
	 	str('r15d') | str('r15')
 	}

	rule(:operand_integer) { str('$') >> match['0-9'].repeat(1).as(:int) >> space? }
	# rule(:shift) { match('-\d+\(\w+\)') }
	rule(:operand_register) { (str('-').maybe >> match['0-9'].repeat).as(:shift) >> lparen >> str('%') >>
								 	register.as(:register) >> rparen >> space? |
									 	str('%') >> register.as(:register) >> space?  }
	rule(:instruction) { match['a-z'].repeat(1) }

	rule(:operand) { operand_integer | operand_register }
	rule(:operand_list) { operand >> (comma >> operand).repeat }
	#rule(:line) { instruction | instruction >> space >> arglist }
	rule(:line) { instruction.as(:instruction) >> space? >> operand_list.as(:operand_list).maybe }
	rule(:label) { match['a-z_'] >> str(':') }

	rule(:expression) { line | label }
	#rule(:integer) { match('[0-9]').repeat(1) }
	root(:expression)
end

class Machine
	def initialize
		@register = Hash.new(0)
		@memory = Hash.new(0)
		@stack = []
	end

	def cal_ptr addr
		@register[addr.key()] + addr.shift
	end

	def set addr, value
		if addr.is_ptr?
			@memory[cal_ptr(addr)] = value
		else
			@register[addr.key()] = value
		end
	end

	def get addr
		return addr unless addr.is_a? Address
		#return @memory[cal_ptr(addr)] if addr.is_ptr?
		return @register[cal_ptr(addr)] if addr.is_ptr?
		return @register[addr.key()]
	end

	def mov args
		from = args[0]
		to = args[1]
		v = get(from)

		set(to, v)
	end

	def push addr
		@stack.push(get(addr))
	end

	def pop addr
		set(addr, @stack.pop())
	end

	def add args
		set(args[1], get(args[0]) + get(args[1]))
	end

	def imul args
		set(args[1], get(args[0]) * get(args[1]))
	end

	def ret args
		get(Address.new("eax"))
	end
end

VM = Machine.new

class Instruction
	def initialize name, args=[]
		@name = name
		@args = args
	end

	def eval
		VM.method(@name.to_s.intern).call(@args)
=begin
		if @name == "mov"
			VM.mov(@args)
		elsif @name == "push"
			VM.push(@args)
		elsif @name == "pop"
			VM.pop(@args)
		end
=end
	end
end

class Address
	def initialize label, shift=0
		@label = label
		@shift = shift
	end

	attr_reader :label, :shift

	def is_ptr?
		@label =~ /bp$/
	end

	def key
		label = @label.to_s
		label = label[label.length - 2, 2]
		if @shift.to_i < 0
			"#{label}#{@shift}"
		else
			"#{label}+#{@shift}"
		end
	end
end

class AsmTransform < Parslet::Transform
	rule(:register => simple(:i), :shift => simple(:s)) { Address.new(i, s.to_i) }
	rule(:register => simple(:i)) { Address.new(i) }
	rule(:int => simple(:i)) { i.to_i }
	rule(:instruction => simple(:i)) { Instruction.new(i) }
	rule(:instruction => simple(:i), :operand_list => subtree(:a)) { Instruction.new(i, a) }
end

parser = AsmParser.new
transform = AsmTransform.new



#ast = tranform.apply(parser.parse(code))
=begin
lines =<<EOS
mov
pop $1
pop %rbx
mov $2, -4(%rbp)
EOS

lines.split(/\n/).each do |ln|
	ast = transform.apply(parser.parse(ln))
	p ast

	ast.eval
end
=end

is_main = false
cnt = []
#open("tmp.s").each do |ln|
open(ARGV.first).each do |ln|
	ln = ln.strip.chomp
	if ln =~ /mymain:/
		is_main = true
		next
	end
	next if is_main == false
	next if ln.size == 0
	cnt << ln
end

ret = nil
cnt.each do |code|
	ast = transform.apply(parser.parse(code))
	ret = ast.eval
end
p VM
puts ret

