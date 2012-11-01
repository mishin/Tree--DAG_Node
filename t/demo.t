#!/usr/bin/env perl

use strict;
use warnings;

use Tree::DAG_Node;

my($starting_node);

# -----------------------------------------------

sub grow_tree
{
	my($count) = 0;
	my($tree)  = Tree::DAG_Node -> new({name => 'Root', attributes => {'#' => $count} });
	my(%child) =
	(
		I => 'J',
		H => 'J',
		J => 'L',
		L => 'M',
		D => 'F',
		E => 'F',
		F => 'G',
		B => 'C',
	);

	my($child);
	my($kid_1, $kid_2);
	my($name, $node);

	for $name (qw/I H J J L D E F B/)
	{
		$count++;

		$node  = Tree::DAG_Node -> new({name => $name, attributes => {'#' => $count} });
		$child = Tree::DAG_Node -> new({name => $child{$name}, attributes => {'#' => $count} });

		$starting_node = $node if ($name eq 'I');

		$child -> name('K') if ($count == 3);

		if ($child{$name} eq 'M')
		{
			$kid_1 = Tree::DAG_Node -> new({name => 'N', attributes => {'#' => $count}});
			$kid_2 = Tree::DAG_Node -> new({name => 'O', attributes => {'#' => $count}});

			$kid_1 -> add_daughter($kid_2);
			$child -> add_daughter($kid_1);
		}

		$node -> add_daughter($child);
		$tree -> add_daughter($node);
	}

	return $tree;

} # End of grow_tree.

# -----------------------------------------------

sub process_tree_helper
{
	my($tree)      = @_;
	my(@ancestors) = map{$_ -> name} $tree -> daughters;

	my(%ancestors);

	@ancestors{@ancestors} = (1) x @ancestors;

	my($attributes);
	my($name);
	my(@stack);

	$tree -> walk_down
	({
		ancestors => \%ancestors,
		callback  =>
		sub
		{
			my($node, $options) = @_;

			if ($$options{_depth} > 1)
			{
				$attributes = $node -> attributes;
				$name       = $node -> name;

				if (defined $$options{ancestors}{$name} && ! $$attributes{replaced})
				{
					push @{$$options{stack} }, $node;
				}
			}

			return 1;
		},
		_depth => 0,
		stack  => \@stack,
	});

	my($sub_tree) = Tree::DAG_Node -> new;

	my(@kids);
	my($node);
	my(%seen);

	for $node (@stack)
	{
		$name        = $node -> name;
		@kids        = grep{$_ -> name eq $name} $tree -> daughters;
		$seen{$name} = 1;

		$sub_tree -> add_daughters(map{$_ -> copy_at_and_under({no_attribute_copy => 1})} @kids);

		for ($sub_tree -> daughters)
		{
			$_ -> attributes({%{$_ -> attributes}, replaced => 1});
		}

		$node -> replace_with($sub_tree -> daughters);
	}

	return ({%seen}, $#stack);

} # End of process_tree_helper.

# ------------------------------------------------

sub process_tree
{
	my($tree)     = @_;
	my($finished) = 0;

	my(@result);
	my(%seen);

	while (! $finished)
	{
		@result   = process_tree_helper($tree);
		$seen{$_} = 1 for keys %{$result[0]};
		$finished = $result[1] < 0;
	}

	for my $child ($tree -> daughters)
	{
		$tree -> remove_daughter($child) if ($seen{$child -> name});
	}

} # End of process_tree.

# -----------------------------------------------

my($tree) = grow_tree;

print map{"$_\n"} @{$tree -> draw_ascii_tree};
print '-' x 35, "\n";
print map{"$_\n"} @{$tree -> tree2string};
print '-' x 35, "\n";
print map{"$_\n"} @{$tree -> tree2string({no_attributes => 1})};
print '-' x 35, "\n";

process_tree($tree);

print map{"$_\n"} @{$tree -> draw_ascii_tree};
print '-' x 35, "\n";
print map{"$_\n"} @{$tree -> tree2string};
print '-' x 35, "\n";
print map{"$_\n"} @{$tree -> tree2string({no_attributes => 1})};
print '-' x 35, "\n";
print map{"$_\n"} @{$tree -> tree2string({}, $starting_node)};
print '-' x 35, "\n";
