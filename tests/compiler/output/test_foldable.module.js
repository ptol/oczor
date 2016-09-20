oc.test_foldable = {
  item1 : oc.testing.checkEq(oc.instances.eq.Bool,oc.std.foldable.and(oc.instances.foldl.Array,[true,false]),false),
  item2 : oc.testing.checkEq(oc.instances.eq.Bool,oc.std.foldable.or(oc.instances.foldl.Array,[true,false]),true),
  item3 : oc.testing.checkEq(oc.instances.eq.Int,oc.std.foldable.sum(oc.instances.foldl.Array,oc.instances.zero.Int,oc.instances.add.Int,[1,2,3,4]),10),
  item4 : oc.testing.checkEq(oc.instances.eq.Int,oc.std.foldable.product(oc.instances.foldl.Array,oc.instances.one.Int,oc.instances.mul.Int,[1,2,3,4]),24),
  item5 : oc.testing.checkEq(oc.instances.eq.Bool,oc.std.foldable.contains(oc.instances.eq.Int,oc.instances.foldl.Array,3,[1,2,3,4]),true),
  item6 : oc.testing.checkEq(oc.instances.eq.Bool,oc.std.foldable.contains(oc.instances.eq.Int,oc.instances.foldl.Array,5,[1,2,3,4]),false),
  item7 : oc.testing.checkEq(oc.instances.eq.Int,oc.std.foldable.count(oc.instances.foldl.Array,[1,4,5]),3),
  item8 : oc.testing.checkEq(oc.instances.eq.String,oc.std.foldable.concat(oc.instances.foldl.Array,oc.instances.mempty.String,oc.instances.append.String,["123","456","789"]),"123456789")
};