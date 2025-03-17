/*
 * This program underscores the point that polymorhpism only works with pointers
 * and references. This is obvious once I start doing a bit of C++, but coming
 * from maanged languages this was a bit of surprise.
 */

#include <iostream>

class Polygon {
protected:
    int m_height;
    int m_width;

public:
    void setValues(int width, int height)
    {
        m_height = height;
        m_width = width;
    }
};

class Rectangle : public Polygon {
public:
    int area() { return m_height * m_width; }
};

class Triangle : public Polygon {
public:
    int area() { return (m_height * m_width) / 2; }
};

int main(int argc, char** argv)
{
    Rectangle rectangle;
    Triangle triangle;

    // !!! THIS EXPLANATION IS WRONG !!!
    // IT DOES NOT WORK BECAUSE IT WILL CALL POLYGON'S COPY CTOR
    // p2 IS A COMPLETELY DIFFERENT OBJECT.
    // This won't work. Polymorphism only works with pointers and references. This
    // is because it will first upcast rectangle to polygon and then call
    // Polygon's copy ctor to create p1. Any changes made to p1 will not show up
    // in rectangle. Polygon p1 = rectangle; Polygon p2 = triangle;
    // p1.setValues(4, 5);
    // p2.setValues(4, 5);

    // Either of these two will work -

    // Polygon *p1 = &rectangle;
    // Polygon *p2 = &triangle;
    // p1->setValues(4, 5);
    // p2->setValues(4, 5);

    Polygon& p1 = rectangle;
    Polygon& p2 = triangle;
    p1.setValues(4, 5);
    p2.setValues(4, 5);

    int rarea = rectangle.area();
    int tarea = triangle.area();
    std::cout << "Rectangle area: " << std::to_string(rarea) << std::endl;
    std::cout << "Triangle area: " << std::to_string(tarea) << std::endl;
}
