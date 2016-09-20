oc.test_prelude = {
  item1 : oc.testing.checkEq(oc.instances.eq.Bool,(! false),true),
  item2 : oc.testing.checkEq(oc.instances.eq.Bool,(true && false),false),
  item3 : oc.testing.checkEq(oc.instances.eq.Bool,(true || false),true),
  item4 : oc.testing.checkEq(oc.instances.eq.String,oc.instances.show.Int(1),"1"),
  item5 : oc.testing.checkEq(oc.instances.eq.String,oc.instances.append.String("123","456"),"123456")
};