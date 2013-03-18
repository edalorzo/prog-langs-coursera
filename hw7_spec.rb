require "rspec"
require './hw7'

describe Point do

  it "should return a copy of itself" do

    pointOne = Point.new(1.0,1.0)
    pointTwo = pointOne.preprocess_prog

    pointTwo.x.should == 1.0
    pointTwo.y.should == 1.0
  end

  it "should shift a point" do
    shifted = Point.new(1,1).shift(1,1)
    shifted.x.should == 2
    shifted.y.should == 2
  end

  it "should evaluate a point" do
    point = Point.new(1,2).preprocess_prog.eval_prog([])
    point.x.should == 1
    point.y.should == 2
  end

  it "should intersect two points" do
    point = Point.new(1,1).intersect(Point.new(1,1))
    point.x.should == 1
    point.y.should == 1
  end

  it "should intersect into NoPoint" do
    nopoint = Point.new(1,1).intersect(Point.new(2,2))
    nopoint.should be_kind_of(NoPoints)
  end

  it "should intersect into line" do
    point = Point.new(1,2).intersect(Line.new(1,1))
    point.x.should == 1
    point.y.should == 2
  end

  it "should intersect a point and vertical line" do
    point = Point.new(1,2).intersect(VerticalLine.new(1))
    point.x.should == 1
    point.y.should == 2

    nopoint = Point.new(1,1).intersect(VerticalLine.new(3))
    nopoint.should be_kind_of(NoPoints)
  end

end



describe Line do
  it "should return a copy of itself" do
    lineOne = Line.new(1.0,2.0)
    lineTwo = lineOne.preprocess_prog

    lineTwo.m.should == 1.0
    lineTwo.b.should == 2.0
  end

  it "should shift a line" do
    line = Line.new(1,2).shift(2,3);
    line.m.should == 1
    line.b.should == 3
  end

  it "should evaluate a line" do
    line = Line.new(1,2).preprocess_prog.eval_prog([])
    line.m.should == 1
    line.b.should == 2
  end

  it "should intersect into another point" do
    point = Line.new(1,1).intersect(Point.new(1,2))
    point.x.should == 1
    point.y.should == 2
  end

  it "should intersect line against line" do
    line = Line.new(1,2).intersect(Line.new(1,2))
    line.m.should == 1
    line.b.should == 2

    point = Line.new(2,1).intersect(Line.new(1,2))
    point.x.should == 1
    point.y.should == 3
  end

  it "should intersect a line against a vertical line" do
    point = Line.new(1,2).intersect(VerticalLine.new(2))
    point.x.should == 2
    point.y.should == 4
  end

end

describe VerticalLine do
  it "should return a copy of iteself" do
    verticalLineOne = VerticalLine.new(1.0)
    verticalLineTwo = verticalLineOne.preprocess_prog
    verticalLineTwo.x.should == 1.0
  end

  it "should shift a vertical line" do
    vline = VerticalLine.new(1).shift(1,1)
    vline.x.should == 2
  end

  it "should evaluate a vertical line" do
    vline = VerticalLine.new(1).preprocess_prog.eval_prog([])
    vline.x.should == 1
  end


  it "should intersect a vertical line and a point" do
    point = VerticalLine.new(1).intersect(Point.new(1,2))
    point.x.should == 1
    point.y.should == 2

    nopoint = VerticalLine.new(3).intersect(Point.new(1,1))
    nopoint.should be_kind_of(NoPoints)
  end

  it "should intersect a vertical line against a line" do
    point = VerticalLine.new(2).intersect(Line.new(1,2))
    point.x.should == 2
    point.y.should == 4
  end

  it "should intersect two vertical lines" do
    vline = VerticalLine.new(2).intersect(VerticalLine.new(2))
    vline.x.should == 2
  end

end

describe LineSegment do
  it "should return a Point" do
    lineSegment = LineSegment.new(1.0,2.0,1.0,2.0)
    point = lineSegment.preprocess_prog
    point.x.should == 1.0
    point.y.should == 2.0
  end

  it "should reorder coordinates" do
    ls1 = LineSegment.new(1.0,1.0,0.0,0.0).preprocess_prog
    ls1.x1.should == 0.0
    ls1.y1.should == 0.0
    ls1.x2.should == 1.0
    ls1.y2.should == 1.0
    ls2 = LineSegment.new(1.0,1.0,1.0,0.0).preprocess_prog
    ls2.x1.should == 1.0
    ls2.y1.should == 0.0
    ls2.x2.should == 1.0
    ls2.y2.should == 1.0
    ls3 = LineSegment.new(1.0,1.0,0.0,2.0).preprocess_prog
    ls3.x1.should == 0.0
    ls3.y1.should == 2.0
    ls3.x2.should == 1.0
    ls3.y2.should == 1.0
    ls4 = LineSegment.new(1.0,1.0,0.0,1.0).preprocess_prog
    ls4.x1.should == 0.0
    ls4.y1.should == 1.0
    ls4.x2.should == 1.0
    ls4.y2.should == 1.0
  end

  it "keep coordinates in the same order" do
    ls1 = LineSegment.new(1.0,1.0,2.0,0.0).preprocess_prog
    ls1.x1.should == 1.0
    ls1.y1.should == 1.0
    ls1.x2.should == 2.0
    ls1.y2.should == 0.0
    ls2 = LineSegment.new(1.0,1.0,2.0,1.0).preprocess_prog
    ls2.x1.should == 1.0
    ls2.y1.should == 1.0
    ls2.x2.should == 2.0
    ls2.y2.should == 1.0
    ls3 = LineSegment.new(1.0,1.0,2.0,2.0).preprocess_prog
    ls3.x1.should == 1.0
    ls3.y1.should == 1.0
    ls3.x2.should == 2.0
    ls3.y2.should == 2.0
    ls3 = LineSegment.new(1.0,1.0,1.0,2.0).preprocess_prog
    ls3.x1.should == 1.0
    ls3.y1.should == 1.0
    ls3.x2.should == 1.0
    ls3.y2.should == 2.0
  end

  it "shifts a line segment" do
    ls = LineSegment.new(1,1,2,2).shift(2,2);
    ls.x1.should == 3
    ls.y1.should == 3
    ls.x2.should == 4
    ls.y2.should == 4
  end

  it "evaluates a line segment" do
    ls = LineSegment.new(1,1,2,2).preprocess_prog.eval_prog([])
    ls.x1.should == 1
    ls.y1.should == 1
    ls.x2.should == 2
    ls.y2.should == 2
  end

  it "intersects a line segment and a line" do
    point = LineSegment.new(1,1,2,2).intersect(Point.new(1,1))
    point.x.should == 1
    point.y.should == 1
  end


end

describe Var do
  env = [["x",Point.new(1.0,1.0)],["y",LineSegment.new(1.0,2.0,2.0,3.0)]]
  it "should contain variable" do

    var1 = Var.new("x");
    point = var1.eval_prog(env)
    point.x.should == 1.0
    point.y.should == 1.0

    var2 = Var.new("y")
    ls = var2.eval_prog(env)
    ls.x1.should == 1.0
    ls.y1.should == 2.0
    ls.x2.should == 2.0
    ls.y2.should == 3.0
  end

  it "should return a Point" do
    point = Var.new("x").preprocess_prog.eval_prog([["x",Point.new(1,1)]])
    point.x.should == 1
    point.y.should == 1
  end

  it "shifts a var to a var" do
    point = Var.new("x").shift(1,1).preprocess_prog.eval_prog([["x",Point.new(1,1)]])
    point.x.should == 1
    point.y.should == 1
  end

end

describe Let do
  it "should evaluate expression" do

    let = Let.new("x",Point.new(1.0,1.0), Var.new("x"))
    point1 = let.eval_prog([])

    point1.x.should == 1.0
    point1.y.should == 1.0

    point2 = Let.new("x", Shift.new(3,3,LineSegment.new(1,1,1,1)),Var.new("x")).preprocess_prog.eval_prog([])
    point2.x.should == 4
    point2.y.should == 4
  end

  it "shifts a let" do
    let = Let.new("x",Point.new(1.0,1.0), Var.new("x")).shift(1,1)
    point1 = let.preprocess_prog.eval_prog([])
    point1.x.should == 1.0
    point1.y.should == 1.0

  end
end

