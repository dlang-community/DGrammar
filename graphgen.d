import std.d.lexer;
import std.stdio;
import std.range;
import std.conv;
import std.ascii;
import std.string;

class Rule
{
	string name;
	SubRule subRule;
	string nodeName;

	void assignId(string id)
	{
		nodeName = id;
		subRule.id = format("%s_0", id);
		subRule.assignIds();
	}

	void print()
	{
		const string startName = nodeName ~ "start";
		const string endName = nodeName ~ "end";
		printDotNode(startName);
		printDotNode(endName);
		subRule.print([startName]);
//			foreach (sr; subRule.firstIds())
//				printArrow(startName, sr);
		foreach (sr; subRule.lastIds())
			printArrow(sr, endName);
	}
}

class SubRule
{
	Qualifier qualifier;
	SubRule[] parts;
	string termOrRule;
	SubRuleType type;
	string id;

	void print(string[] startIds)
	{
		if (type == SubRuleType.terminal || type == SubRuleType.rule)
		{
			if (type == SubRuleType.terminal)
				printTerminalNode(id, termOrRule);
			if (type == SubRuleType.rule)
				printRuleNode(id, termOrRule);
			foreach (startId; startIds)
				printArrow(startId, id);
		}
		else if (type == SubRuleType.alternatives)
		{
			foreach (sub; parts)
				sub.print(startIds);
		}
		else if (type == SubRuleType.sequence)
		{
			foreach (i; 0 .. parts.length)
			{
				auto sub = parts[i];
				if (i == 0)
					sub.print(startIds);
				else
					sub.print(parts[i - 1].lastIds());
			}
		}
	}

	void assignIds()
	{
		foreach (i, part; parts)
		{
			part.id = format("%s_%d", id, i);
			part.assignIds();
		}
	}

	string[] firstIds()
	{
		final switch (type)
		{
		case SubRuleType.invalid:
			assert (false, "invalid Node");
		case SubRuleType.terminal:
		case SubRuleType.rule:
			return [id];
		case SubRuleType.alternatives:
			string[] ids;
			foreach (rule; parts)
				ids ~= rule.firstIds();
			return ids;
		case SubRuleType.sequence:
			return parts[0].firstIds();
		}
	}

	string[] lastIds()
	{
		final switch (type)
		{
		case SubRuleType.invalid:
			assert (false, "invalid Node " ~ termOrRule);
		case SubRuleType.terminal:
		case SubRuleType.rule:
			return [id];
		case SubRuleType.alternatives:
			string[] ids;
			foreach (rule; parts)
				ids ~= rule.lastIds();
			return ids;
		case SubRuleType.sequence:
			return parts[$ - 1].lastIds();
		}
	}
}

enum Qualifier
{
	none,
	plus,
	star,
	question
}

enum SubRuleType
{
	invalid,
	terminal,
	rule,
	sequence,
	alternatives,
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
		auto rule = parseRule(tokens);
		rule.assignId(format("rule_%d", i));
		rule.print();
	}

	writeln("}");
}

Rule parseRule(T)(ref T tokens)
{
	Rule r = new Rule;
	r.name = tokens.front().value;
	tokens.popFront();
	assert (tokens.front == TokenType.colon, tokens.front.value);
	tokens.popFront();
	r.subRule = parseSubRule(tokens);
	return r;
}

SubRule parseSubRule(T)(ref T tokens)
{
	SubRule rule = new SubRule;

	while (true)
	{
		if (tokens.empty || tokens.front == TokenType.rParen
			|| tokens.front == TokenType.semicolon)
		{
			return rule;
		}

		if (tokens.front == TokenType.identifier)
		{
			auto r = new SubRule;
			r.type == SubRuleType.rule;
			r.termOrRule = tokens.front.value;
		}
	}
	if (tokens.front == TokenType.identifier)
	{

	}

	return rule;
}


void printTerminalNode(string nodeName, string terminal)
in
{
	assert(nodeName.length > 0);
	assert(terminal.length > 0);
}
body
{
	writeln(nodeName, `[shape=oval, label="`, terminal.replace(`"`, `\"`), `"]`);
}

void printRuleNode(string nodeName, string ruleName)
{
	writeln(nodeName, `[shape=rectangle, label="`, ruleName.replace(`"`, `\"`), `"]`);
}

void printDotNode(string nodeName)
{
	writeln(nodeName, `[shape=point, label=""]`);
}

void printArrow(string src, string dst)
{
	writefln("%s -> %s [arrowhead=none, tailport=e, headport=w]", src, dst);
}
