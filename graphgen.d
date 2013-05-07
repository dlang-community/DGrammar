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
	abstract void print(File f, string[] startIds);
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
		alternatives.assignIds(id ~ "_0");
	}

	override void print(File f, string[] startIds)
	{
		string startDot = format("%s_start", id);
		string endDot = format("%s_end", id);
        printDotNode(f, startDot);
        printDotNode(f, endDot);
		alternatives.print(f, [startDot]);
		foreach (end; alternatives.getEndIds())
			printArrow(f, end, endDot);
	}

	override string[] getStartIds() { return null; }
	override string[] getEndIds() { return null; }
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

	override void print(File f, string[] startIds)
	{
        if (alternatives.length > 1)
        {
            printDotNode(f, startDot);
            printDotNode(f, endDot);
            foreach (start; startIds)
                printArrow(f, start, startDot);
            foreach (alt; alternatives)
            {
                alt.print(f, [startDot]);
                foreach (end; alt.getEndIds())
                    printArrow(f, end, endDot);
            }
        }
        else
        {
            alternatives[0].print(f, startIds);
        }
	}

	override string[] getStartIds()
	{
		return [startDot];
	}

	override string[] getEndIds()
	{
		if (alternatives.length > 1)
            return [endDot];
        else
            return alternatives[0].getEndIds();
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

    override void print(File f, string[] startIds)
    {
        if (items.length > 1) f.writeln("subgraph cluster_", id, " {\nstyle=invis");
        for (int i = 0; i < items.length; i++)
        {
            if (i == 0)
                items[0].print(f, startIds);
            else
                items[i].print(f, items[i - 1].getEndIds());
        }
        if (items.length > 1) f.writeln("}");
    }

    override string[] getStartIds()
    {
        return items[0].getStartIds();
    }

    override string[] getEndIds()
    {
        return items[$ - 1].getEndIds();
    }
}

class Terminal : Node
{
	string terminal;

	override void assignIds(string id)
	{
		this.id = id;
	}

	override void print(File f, string[] startIds)
	{
		f.writeln(id, `[shape=rectangle, style=rounded, label="`, terminal.replace(`"`, `\"`), `"]`);
		foreach (s; startIds)
            f.writefln("%s -> %s [weight=9001]", s, id);

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

	override void print(File f, string[] startIds)
	{
		f.writeln(id, `[shape=rectangle, label="`, ruleReference.replace(`"`, `\"`), `"]`);
		foreach (s; startIds)
			f.writefln("%s -> %s [weight=9001]", s, id);//printArrow(s, id);
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

    override void assignIds(string id)
    {
        this.id = id;
        repeatStart = id ~ "_repeatStart";
        repeatEnd = id ~ "_repeatEnd";
        optionStart = id ~ "_optionStart";
        optionEnd = id ~ "_optionEnd";
        node.assignIds(id ~ "_0");
    }

	override void print(File f, string[] startIds)
	{
		final switch (qualifier)
		{
			case Qualifier.star:
				printDotNode(f, optionStart);
                printDotNode(f, optionEnd);

                f.writeln("subgraph cluster_", id, "_repeat {\nstyle=invis");
                printDotNode(f, repeatEnd);
                printDotNode(f, repeatStart);
                node.print(f, [repeatStart]);
                f.writefln("%s -> %s [dir=back]", repeatStart, repeatEnd);
                f.writeln("}");

                foreach (start; startIds)
					f.writefln("%s -> %s [weight=9001]", start, optionStart);

                f.writefln("%s -> %s [weight=9001]", optionStart, repeatStart);
                foreach (end; node.getEndIds())
					f.writefln("%s -> %s [weight=9001]", end, repeatEnd);
				f.writefln("%s -> %s [weight=9001]", repeatEnd, optionEnd);
				f.writefln("%s -> %s", optionStart, optionEnd);
				break;
			case Qualifier.question:
				f.writeln("subgraph cluster_", id, "_repeat {\nstyle=invis");
                printDotNode(f, optionStart);
                printDotNode(f, optionEnd);
                node.print(f, [optionStart]);
                f.writeln("}");

                foreach (start; startIds)
					f.writefln("%s -> %s [weight=9001]", start, optionStart);
				f.writefln("%s -> %s", optionStart, optionEnd);
				foreach (end; node.getEndIds())
					f.writefln("%s -> %s [weight=9001]", end, optionEnd);
				break;
			case Qualifier.plus:
                f.writeln("subgraph cluster_", id, "_repeat {\nstyle=invis");
                printDotNode(f, repeatStart);
                printDotNode(f, repeatEnd);
                node.print(f, [repeatStart]);
                f.writeln("}");

				foreach (start; startIds)
					f.writefln("%s -> %s [weight=9001]", start, repeatStart);
				foreach (end; node.getEndIds())
					f.writefln("%s -> %s [weight=9001]", end, repeatEnd);
                f.writefln("%s -> %s [dir=back]", repeatStart, repeatEnd);
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


    //writeln("concentrate=true");
    //writeln("splines=ortho");
    //writeln("splines=spline");

    for (int i = 0; !tokens.empty; i++)
    {
        auto rule = parseRuleDefinition(tokens);
        auto o = File(rule.name ~ ".dot", "w");
        o.write(q"[digraph grammar
{
rankdir=LR
fontsize=10
fontname="Liberation Mono"
node [fontsize=10, fontname="Liberation Mono", margin=0.1, height=0.3]
ranksep=0.2
edge [arrowhead=vee, arrowsize=0.5, len=0.2]
]");
        o.writeln("label=\"", rule.name, "\"");
        rule.assignIds(format("rule%d", i));
        rule.print(o, null);
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
	f.writeln(nodeName, `[shape=point, label="", width=0, height=0, fixedsize=true]`);
}

void printArrow(File f, string src, string dst, bool arrowHead = true)
{
    if (arrowHead)
        f.writefln("%s -> %s [arrowhead=vee]", src, dst);
    else
        f.writefln("%s -> %s", src, dst);
}
