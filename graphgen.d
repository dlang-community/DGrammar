import std.d.lexer;
import std.stdio;
import std.range;
import std.conv;
import std.ascii;
import std.string;

class Node
{
	string id;
	abstract void assignIds(string id);
	abstract void print(string[] startIds);
	abstract string[] getStartIds();
	abstract string[] getEndIds();
}

class RuleDefinition : Node
{
	string name;
	Alternatives alternatives;

	override void assignIds(string id)
	{
		this.id = id;
		alternatives.assignIds(id);
	}

	override void print(string[] startIds)
	{
		string startDot = format("%s_start", id);
		string endDot = format("%s_end", id);
		alternatives.print([startDot]);
		foreach (end; alternatives.getEndIds())
			printArrow(end, endDot);
	}

	override string[] getStartIds() { return null; }
	override string[] getEndIds() { return null; }
}

class Alternatives : Node
{
	Node[] alternatives;

	override void assignIds(string id)
	{
		foreach (i, alt; alternatives)
			alt.assignIds(format("%s_%d", id, i));
	}

	override void print(string[] startIds)
	{
		foreach (alt; alternatives)
		{
			alt.print(startIds);
			foreach (start; startIds)
				printArrow(start, alt.id);
		}
	}

	override string[] getStartIds()
	{
		string[] startIds;
		foreach (alt; alternatives)
		{
			startIds ~= alt.getStartIds();
		}
		return startIds;
	}

	override string[] getEndIds()
	{
		string[] endIds;
		foreach (alt; alternatives)
		{
			endIds ~= alt.getEndIds();
		}
		return endIds;
	}
}

class Sequence : Node
{
	Node[] items;
}

class Terminal : Node
{
	string terminal;

	override void assignIds(string id)
	{
		this.id = id;
	}

	override void print(string[] startIds)
	{
		writeln(id, `[shape=oval, label="`, terminal.replace(`"`, `\"`), `"]`);
		foreach (s; startIds)
			printArrow(s, id);
	}

	override string[] getStartIds() { return [id]; }

	override string[] getEndIds() { return [id]; }
}

class RuleReference : Node
{
	string ruleReference;

	override void assignIds(string id)
	{
		this.id = id;
	}

	override void print(string[] startIds)
	{
		writeln(id, `[shape=rectangle, label="`, ruleReference.replace(`"`, `\"`), `"]`);
		foreach (s; startIds)
			printArrow(s, id);
	}

	override string[] getStartIds() { return [id]; }

	override string[] getEndIds() { return [id]; }
}

class OptionOrRepeat : Node
{
	Node node;
	Qualifier qualifier;
	string repeatStart;
	string repeatEnd;
	string optionStart;
	string optionEnd;

	override void print(string[] startIds)
	{
		final switch (qualifier)
		{
			case Qualifier.star:
				foreach (start; startIds)
					printArrow(start, optionStart);
				printArrow(optionStart, repeatStart);
				node.print([repeatStart]);
				foreach (end; node.getEndIds())
					printArrow(end, repeatEnd);
				printArrow(repeatEnd, repeatStart);
				printArrow(repeatEnd, optionEnd);
				printArrow(optionStart, optionEnd);
				break;
			case Qualifier.question:
				foreach (start; startIds)
					printArrow(start, optionStart);
				printArrow(optionStart, optionEnd);
				node.print([optionStart]);
				foreach (end; node.getEndIds())
					printArrow(end, optionEnd);
				break;
			case Qualifier.plus:
				foreach (start; startIds)
					printArrow(start, repeatStart);
				node.print([repeatStart]);
				foreach (end; node.getEndIds())
					printArrow(end, repeatEnd);
				break;
		}
	}

	override string[] getStartIds()
	{
		if (qualifier == Qualifier.star || qualifier == Qualifier.question)
			return [optionStart];
		else
			return [repeatStart];
	}

	override string[] getEndIds()
	{
		if (qualifier == Qualifier.star || qualifier == Qualifier.question)
			return [optionEnd];
		else
			return [repeatEnd];
	}

}

enum Qualifier
{
	star,
	question,
	plus
}

void main(string[] args)
{
	LexerConfig config;
	config.tokenStyle = TokenStyle.source;
	auto f = File(args[1]);
	auto tokens = (cast(ubyte[]) f.byLine(KeepTerminator.yes).join()).byToken(config);

	writeln(q"[digraph grammar
{
	rankdir=LR
	fontsize=10
	nodesep=0.1
	]");//splines=ortho

	for (int i = 0; !tokens.empty; i++)
	{
	}

	writeln("}");
}


void printDotNode(string nodeName)
{
	writeln(nodeName, `[shape=point, label=""]`);
}

void printArrow(string src, string dst)
{
	writefln("%s -> %s [arrowhead=none, tailport=e, headport=w]", src, dst);
}
