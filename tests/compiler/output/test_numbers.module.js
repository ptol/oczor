oc.test_numbers = {
  item1 : oc.testing.checkEq(oc.instances.eq.Bool,(1 === 2),false),
  item2 : oc.testing.checkEq(oc.instances.eq.Int,((1 + 2) * 3),9),
  item3 : oc.testing.checkEq(oc.instances.eq.Int,(1 + 2),3),
  item4 : oc.testing.checkEq(oc.instances.eq.Int,(2 * 3),6),
  item5 : oc.testing.checkEq(oc.instances.eq.Int,(9 - 5),4),
  item6 : oc.testing.checkEq(oc.instances.eq.Int,oc.instances.div.Int(12,4),3),
  item7 : oc.testing.checkEq(oc.instances.eq.Double,(1.0 + 2.0),3.0),
  item8 : oc.testing.checkEq(oc.instances.eq.Double,(2.0 * 3.0),6.0),
  item9 : oc.testing.checkEq(oc.instances.eq.Double,(9.0 - 5.0),4.0),
  item10 : oc.testing.checkEq(oc.instances.eq.Double,(12.0 / 4.0),3.0)
};