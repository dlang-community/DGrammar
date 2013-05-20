/**
 * Written by Brian Schott (@Hackerpilot on github)
 *
 * Licensed under the WTFPL
 */

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
	abstract void print(File f);
	abstract string getStartId();
	abstract string getEndId();
}

class RuleDefinition : Node
{
	string name;
	Alternatives alternatives;

	override void assignIds(string id)
	{
		this.id = id;
		alternatives.assignIds(id ~ "_0");
	}

	override void print(File f)
	{
		string startDot = format("%s_start", id);
		string endDot = format("%s_end", id);
        f.writefln(`%s[shape=point, label="", width=.05, height=.05, fixedsize=true]`, startDot);
        f.writefln(`%s[shape=point, label="", width=.05, height=.05, fixedsize=true]`, endDot);

        //printDotNode(f, startDot);
        //printDotNode(f, endDot);
		alternatives.print(f);
		printHeavyArrow(f, startDot, alternatives.getStartId());
		printHeavyArrow(f, alternatives.getEndId(), endDot);
	}

	override string getStartId() { return null; }
	override string getEndId() { return null; }
}

class Alternatives : Node
{
	Node[] alternatives;
    string startDot;
    string endDot;

	override void assignIds(string id)
	{
        this.id = id;
        startDot = id ~ "start";
        endDot = id ~ "end";
		foreach (i, alt; alternatives)
			alt.assignIds(format("%s_%d", id, i));
	}

	override void print(File f)
	{

        if (alternatives.length > 1)
        {
			//f.writeln("subgraph cluster_", id, " {\nstyle=invis");
            printDotNode(f, startDot);
            printDotNode(f, endDot);
            foreach (i, alt; alternatives)
            {
                alt.print(f);
				if (i > 0)
				{
					printHeavyArrow(f, startDot, alt.getStartId());
					printHeavyArrow(f, alt.getEndId(), endDot);
				}
				else
				{
					printLightArrow(f, startDot, alt.getStartId());
					printLightArrow(f, alt.getEndId(), endDot);
				}
            }
			//f.writeln("}");
        }
        else
        {
            alternatives[0].print(f);
        }
	}

	override string getStartId()
	{
		if (alternatives.length > 1)
			return startDot;
		else
			return alternatives[0].getStartId();
	}

	override string getEndId()
	{
		if (alternatives.length > 1)
            return endDot;
        else
            return alternatives[0].getEndId();
	}
}

class Sequence : Node
{
	Node[] items;

    override void assignIds(string id)
    {
        this.id = id;
        foreach (i, item; items)
            item.assignIds(format("%s_%d", id, i));
    }

    override void print(File f)
    {
		//if (items.length > 1) f.writeln("subgraph cluster_", id, " {\nstyle=invis");
        for (int i = 0; i < items.length; i++)
        {
			items[i].print(f);
            if (i > 0)
                printHeavyArrow(f, items[i - 1].getEndId(), items[i].getStartId());
        }
		//if (items.length > 1) f.writeln("}");
    }

    override string getStartId()
    {
        return items[0].getStartId();
    }

    override string getEndId()
    {
        return items[$ - 1].getEndId();
    }
}

class Terminal : Node
{
	string terminal;

	override void assignIds(string id)
	{
		this.id = id;
	}

	override void print(File f)
	{
		f.writeln(id, `[shape=rectangle, style=rounded, label="`, terminal.replace(`"`, `\"`), `"]`);
	}

	override string getStartId() { return id; }
	override string getEndId() { return id; }
}

class RuleReference : Node
{
	string ruleReference;

	override void assignIds(string id)
	{
		this.id = id;
	}

	override void print(File f)
	{
		f.writeln(id, `[shape=rectangle, label="`, ruleReference.replace(`"`, `\"`), `"]`);
	}

	override string getStartId() { return id; }
	override string getEndId() { return id; }
}

class OptionOrRepeat : Node
{
	Node node;
	Qualifier qualifier;
	string repeatStart;
	string repeatEnd;
	string optionStart;
	string optionEnd;

    override void assignIds(string id)
    {
        this.id = id;
        repeatStart = id ~ "_repeatStart";
        repeatEnd = id ~ "_repeatEnd";
        optionStart = id ~ "_optionStart";
        optionEnd = id ~ "_optionEnd";
        node.assignIds(id ~ "_0");
    }

	override void print(File f)
	{
		//f.writeln("subgraph cluster_", id, "{\nstyle=invis");
		final switch (qualifier)
		{
			case Qualifier.star:
				printDotNode(f, optionStart);
                printDotNode(f, optionEnd);
				printOptionArrow(f, optionStart, optionEnd);
                printDotNode(f, repeatEnd);
                printDotNode(f, repeatStart);
				printHeavyArrow(f, repeatEnd, optionEnd);
				printBackArrow(f, repeatStart, repeatEnd);
				printHeavyArrow(f, optionStart, repeatStart);
                node.print(f);
                printHeavyArrow(f, repeatStart, node.getStartId());
                printHeavyArrow(f, node.getEndId(), repeatEnd);
				break;
			case Qualifier.question:
                printDotNode(f, optionStart);
                printDotNode(f, optionEnd);
				printLightArrow(f, optionStart, optionEnd);
                node.print(f);
				printHeavyArrow(f, optionStart, node.getStartId());
                printHeavyArrow(f, node.getEndId(), optionEnd);
				break;
			case Qualifier.plus:
                printDotNode(f, repeatStart);
                printDotNode(f, repeatEnd);
				printBackArrow(f, repeatStart, repeatEnd);
                node.print(f);
				printHeavyArrow(f, repeatStart, node.getStartId());
                printHeavyArrow(f, node.getEndId(), repeatEnd);
				break;
		}
		//f.writeln("}");
	}

	override string getStartId()
	{
		if (qualifier == Qualifier.star || qualifier == Qualifier.question)
			return optionStart;
		else
			return repeatStart;
	}

	override string getEndId()
	{
		if (qualifier == Qualifier.star || qualifier == Qualifier.question)
			return optionEnd;
		else
			return repeatEnd;
	}

}

enum Qualifier
{
	star,
	question,
	plus
}

RuleDefinition parseRuleDefinition(T)(ref T tokens)
{
    assert (tokens.front == TokenType.identifier || isKeyword(tokens.front));
    auto rd = new RuleDefinition;
    rd.name = tokens.front.value;
    tokens.popFront();
    assert (tokens.front == TokenType.colon);
    tokens.popFront();
    rd.alternatives = parseAlternatives(tokens);
    assert (tokens.front == TokenType.semicolon, tokens.front.value);
    tokens.popFront();
    return rd;
}

Alternatives parseAlternatives(T)(ref T tokens)
{
    auto alt = new Alternatives;
    auto seq = new Sequence;
    while (true)
    {
        if (tokens.front == TokenType.stringLiteral
            || isKeyword(tokens.front)
            || tokens.front == TokenType.identifier)
        {
            Node node;
            if (tokens.front == TokenType.stringLiteral
                || (tokens.front == TokenType.identifier && isUpper(tokens.front.value[0])))
            {
                auto terminal = new Terminal;
                terminal.terminal = tokens.front.value;
                node = terminal;
            }
            else
            {
                auto ruleRef = new RuleReference;
                ruleRef.ruleReference = tokens.front.value;
                node = ruleRef;
            }
            tokens.popFront();
            if (tokens.front == TokenType.star
                || tokens.front == TokenType.ternary
                || tokens.front == TokenType.plus)
            {
                auto option = new OptionOrRepeat;
                if (tokens.front == TokenType.star)
                    option.qualifier = Qualifier.star;
                if (tokens.front == TokenType.ternary)
                    option.qualifier = Qualifier.question;
                if (tokens.front == TokenType.plus)
                    option.qualifier = Qualifier.plus;
                tokens.popFront();
                option.node = node;
                seq.items ~= option;
            }
            else
                seq.items ~= node;
        }
        else if (tokens.front == TokenType.rParen
            || tokens.front == TokenType.semicolon)
        {
            alt.alternatives ~= seq;
            break;
        }
        else if (tokens.front == TokenType.lParen)
        {
            tokens.popFront();
            auto item = parseAlternatives(tokens);
            assert (tokens.front == TokenType.rParen);
            tokens.popFront();

            if (tokens.front == TokenType.star
                || tokens.front == TokenType.ternary
                || tokens.front == TokenType.plus)
            {
                auto option = new OptionOrRepeat;
                if (tokens.front == TokenType.star)
                    option.qualifier = Qualifier.star;
                if (tokens.front == TokenType.ternary)
                    option.qualifier = Qualifier.question;
                if (tokens.front == TokenType.plus)
                    option.qualifier = Qualifier.plus;
                tokens.popFront();
                option.node = item;
                seq.items ~= option;
            }
            else
                seq.items ~= item;
        }
        else if (tokens.front == TokenType.bitOr)
        {
            alt.alternatives ~= seq;
            seq = new Sequence;
            tokens.popFront();
        }
    }
    return alt;
}



void main(string[] args)
{
	LexerConfig config;
	config.tokenStyle = TokenStyle.source;
	auto f = File(args[1]);
	auto tokens = (cast(ubyte[]) f.byLine(KeepTerminator.yes).join()).byToken(config);





    for (int i = 0; !tokens.empty; i++)
    {
        auto rule = parseRuleDefinition(tokens);
        auto o = File(rule.name ~ ".dot", "w");
        o.write(q"[digraph grammar
{
rankdir=LR
fontsize=10
fontname="Liberation Mono"
node [fontsize=10, fontname="Liberation Mono", margin=0.05, height=0.3]
edge [arrowhead=none, arrowsize=0.5, length=0.2]
ranksep=0.2
]");
        o.writeln("label=\"", rule.name, "\"");
        rule.assignIds(format("rule%d", i));
        rule.print(o);
        o.writeln("}");
    }


}

RuleDefinition createTestRule()
{
    auto r = new RuleDefinition;
    auto alt = new Alternatives;
    auto seq = new Sequence;
    auto term1 = new Terminal;
    term1.terminal = `"abcde"`;
    auto term2 = new Terminal;
    term2.terminal = `"fghi"`;
    auto oor = new OptionOrRepeat;
    oor.node = term2;
    oor.qualifier = Qualifier.star;
    seq.items ~= term1;
    seq.items ~= oor;
    alt.alternatives ~= seq;

    auto seq2 = new Sequence;
    auto term3 = new Terminal;
    term3.terminal = "xyz";
    seq2.items ~= term3;
    alt.alternatives ~= seq2;
    r.alternatives = alt;
    return r;
}


void printDotNode(File f, string nodeName)
{
	f.writeln(nodeName, `[shape=point, label="", width=.01, height=.01, fixedsize=true]`);
}

void printHeavyArrow(File f, string src, string dst)
{
	f.writefln("%s -> %s [weight=100]", src, dst);
}

void printLightArrow(File f, string src, string dst)
{
	f.writefln("%s -> %s [weight=0]", src, dst);
}

void printOptionArrow(File f, string src, string dst)
{
	f.writefln("%s -> %s [weight=0, constraint=false]", src, dst);
}

void printBackArrow(File f, string src, string dst)
{
	f.writefln("%s -> %s [arrowstyle=normal, weight=0, constraint=false, dir=back]", src, dst);
}
