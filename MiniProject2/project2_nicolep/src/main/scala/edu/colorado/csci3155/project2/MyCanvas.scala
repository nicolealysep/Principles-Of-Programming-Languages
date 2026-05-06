package edu.colorado.csci3155.project2
import swing._
import java.awt.image.BufferedImage

/* A class to maintain a canvas */
import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{Graphics2D, Color}

sealed trait Figure {
    def getBoundingBox: (Double, Double, Double, Double)
    def translate(x: Double, y: Double): Figure
    def rotate(mat: (Double, Double, Double, Double)): Figure
    def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double): Unit
    def reflectX: Figure
    def reflectY: Figure
    def scale(s: Double): Figure
}

case class Polygon(val cList: List[(Double, Double)]) extends Figure {
    override def getBoundingBox: (Double, Double, Double, Double) = {
       /* TODO: implement getBoundingBox for a polygon. */
       // extract x and y coordinates 
       // x min = min x ....
       //return xmin, xmax, ymin, ymax
       val xList = cList.map { tuple => tuple._1 }
       val yList = cList.map { tuple => tuple._2 }
       val xmin = xList.min
       val xmax = xList.max
       val ymin = yList.min
       val ymax = yList.max
       return (xmin, xmax, ymin, ymax)
    }
    override def translate(x: Double, y: Double): Polygon = {
        val newList = cList.map {case (xc,yc) => (xc+x, yc+y)}
        new Polygon(newList)

    }
    override def rotate(mat: (Double, Double, Double, Double)): Polygon = {
        val nList = cList.map { case (x, y ) => (mat._1 * x + mat._2 * y, mat._3 * x + mat._4 * y)}
        new Polygon(nList)
    }

    override def reflectX: Polygon = {
       /* TODO: implement reflectX for a polygon */
       val list = cList.map { case (x, y) => (x, -y) }
       new Polygon(list)
    }

    override def reflectY: Polygon = {
        /* TODO: reflectY for a polygon */
        val list = cList.map { case (x, y) => (-x, y) }
        new Polygon(list)
    }


    override def scale(s: Double): Figure = {
        /* TODO: implement scale for a polygon */
        val list = cList.map { case (x, y) => (s*x, s*y) }
        new Polygon(list)
    }

    /* WARNING: DO NOT EDIT */
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val xPoints: Array[Int] = new Array[Int](cList.length)
        val yPoints: Array[Int] = new Array[Int](cList.length)
        for (i <- 0 until cList.length){
            xPoints(i) = ((cList(i)._1 + shiftX )* scaleX).toInt
            yPoints(i) = ((cList(i)._2 + shiftY) * scaleY).toInt
        }
        g.drawPolygon(xPoints, yPoints, cList.length)
    }

}

case class MyCircle(val c: (Double, Double), val r: Double) extends Figure {
    override def getBoundingBox: (Double, Double, Double, Double) = {
       /* TODO: implement getBoundingBox for a Circle object */
       // x min = centerx - radius 
       // x max = centerx + radius
       // y min = centery - radius
       // y max = centery + radius
       // return (xmin, xmax, ymin, ymax)

       val cx = c._1
       val cy = c._2
       val xmin = cx - r
       val xmax = cx + r
       val ymin = cy - r
       val ymax = cy + r
       return (xmin, xmax, ymin, ymax)
    }

    override def translate(x: Double, y: Double): MyCircle = {
        val ncenter = (c._1 + x, c._2 + y)
        new MyCircle(ncenter, r)
    }

    override def rotate(mat: (Double, Double, Double, Double)): MyCircle = {
        val newcenter = (c._1 * mat._1 + c._2 * mat._2, c._1 * mat._3 + c._2 * mat._4)
        new MyCircle(newcenter, r)
    }

    override def reflectX: MyCircle = {
        /* TODO : implement reflectX for a circle object*/
        c match {
            case (x, y) => new MyCircle((x, -y), r)
        }
    }

    override def reflectY: Figure = {
        /* TODO */
        c match {
            case (x, y) => new MyCircle((-x, y), r)
        }
    }

    override def scale(s: Double): Figure = {
        /* TODO : implement scale for a circle object*/
        c match {
            case (x, y) => new MyCircle((s*x, s*y), s*r)
        }
    }

    /* WARNING: DO NOT EDIT */
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val centerX = ((c._1 + shiftX) * scaleX) .toInt
        val centerY = ((c._2 + shiftY) * scaleY) .toInt
        val radX = (r * scaleX).toInt
        val radY = (r * math.abs(scaleY)).toInt
        //g.draw(new Ellipse2D.Double(centerX, centerY, radX, radY))
        g.drawOval(centerX-radX, centerY-radY, 2*radX, 2*radY)
    }
}

class MyCanvas (val listOfObjects: List[Figure]) {
    def getBoundingBox: (Double, Double, Double, Double) = {
        /* TODO : Implement getBoundingBox for a Canvas by calling getBoundingBox on each figure in the listOfObjects*/
        // xmin = min of all xmin's... 
        // return combined box 
        // get each figures bounding box
        val xmin = listOfObjects.map { f => f.getBoundingBox._1 }.min
        val xmax = listOfObjects.map { f => f.getBoundingBox._2 }.max
        val ymin = listOfObjects.map { f => f.getBoundingBox._3 }.min
        val ymax = listOfObjects.map { f => f.getBoundingBox._4 }.max
        return (xmin, xmax, ymin, ymax)
    }

    def translate(shiftX: Double, shiftY: Double) = {
        val newList = listOfObjects.map {(f)=> f.translate(shiftX, shiftY)}
        new MyCanvas(newList)
    }

    def placeRight(myc2: MyCanvas):MyCanvas = {
        /* TODO */
        val (xmin1, xmax1, ymin1, ymax1) = this.getBoundingBox
        val (xmin2, xmax2, ymin2, ymax2) = myc2.getBoundingBox

        val shiftX = xmax1 - xmin2
        val centerY1 = (ymin1 + ymax1) / 2
        val centerY2 = (ymin2 + ymax2) / 2
        val shiftY = centerY1 - centerY2

        val shiftedCanvas2 = myc2.translate(shiftX, shiftY)

        this.overlap(shiftedCanvas2)
    }

    def placeTop(myc2: MyCanvas): MyCanvas = {
        /* TODO */
        val (xmin1, xmax1, ymin1, ymax1) = this.getBoundingBox
        val (xmin2, xmax2, ymin2, ymax2) = myc2.getBoundingBox

        val shiftY = ymax1 - ymin2
        val centerX1 = (xmin1 + xmax1) / 2
        val centerX2 = (xmin2 + xmax2) / 2
        val shiftX = centerX1 - centerX2

        val shiftedCanvas2 = myc2.translate(shiftX, shiftY)
        this.overlap(shiftedCanvas2)
    }

    // 2D Rotation about a point (x,y)
    // x' = xcos(theta) - ysin(theta), y' = ycos(theta) + xsin(theta)
    def rotate(angRad: Double): MyCanvas = {
        val mat = (math.cos(angRad), -math.sin(angRad), math.sin(angRad), math.cos(angRad))
        val newList = listOfObjects.map {(f) => f.rotate(mat) }
        new MyCanvas(newList)
    }

    def reflectX: MyCanvas = {
        /* TODO */
        val newList = listOfObjects.map { f => f.reflectX }
        new MyCanvas(newList)
    }

    def reflectY: MyCanvas = {
        /* TODO */
        val newList = listOfObjects.map { f => f.reflectY }
        new MyCanvas(newList)
    }


    /* WARNING: DO NOT EDIT */
    def render(g: Graphics2D, xMax: Double, yMax: Double) = {
         //g.setColor(Color.WHITE)
         //g.fillRect(0, 0, g.getWidth, g.getHeight)
        // enable anti-aliased rendering (prettier lines and circles)
        // Comment it out to see what this does!
        g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING,  java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
        g.setColor(Color.BLACK)
        val (lx1, ux1, ly1, uy1) = this.getBoundingBox
        // Note(klinvill): Previously the code was shifting the figure right and
        // down by 1 unit. I'm unclear on why this was originally needed, but it
        // means that lines without shifting are rendered outside the figure.
        // Removing the extra shift also gets rid of some artifacts where the
        // edges of the rendered objects were cut off.
        val shiftx = -lx1
        val shifty = -uy1
        val scaleX = xMax/(ux1 - lx1 + 1.0)
        val scaleY = yMax/(uy1 - ly1 + 1.0)
        //println(scaleX, scaleY)
        val scale = math.min(scaleX, scaleY)
        listOfObjects.foreach(f => f.render(g,scale, -scale, shiftx, shifty))
    }

    def scale(s: Double): MyCanvas = {
        /* TODO */
        val newList = listOfObjects.map { f => f.scale(s) }
        new MyCanvas(newList)
    }

    def overlap(c2: MyCanvas): MyCanvas = {
        /* TODO */
        val combinedList = listOfObjects ++ c2.getListOfObjects
        new MyCanvas(combinedList)
    }

    override def toString: String = {
        listOfObjects.foldLeft[String] ("") { case (acc, fig) => acc ++ fig.toString }
    }

    def getListOfObjects: List[Figure] = listOfObjects

    def numPolygons: Int =
        listOfObjects.count {
            case Polygon(_) => true
            case _ => false }

    def numCircles: Int = {
        listOfObjects.count {
            case MyCircle(_,_) => true
            case _ => false }
    }

    def numVerticesTotal: Int = {
        listOfObjects.foldLeft[Int](0) ((acc, f) =>
            f match {
                case Polygon(lst1) => acc + lst1.length
                case _ => acc
            }
        )
    }

    def renderImage(filename: String) = {
        val (lx1, ux1, ly1, uy1) = this.getBoundingBox

        val sx = (ux1 - lx1 + 1.0).toInt
        val sy = (uy1 - ly1 + 1.0).toInt
        val t = math.min(1200/sx, 1200/sy).toInt
        val size = (t *  sx, t * sy)
        // create an image
        val canvas = new BufferedImage(size._1, size._2, BufferedImage.TYPE_INT_RGB)
        // get Graphics2D for the image
        val g = canvas.createGraphics()
        // clear background
        g.setColor(Color.WHITE)
        g.fillRect(0, 0, canvas.getWidth, canvas.getHeight)
        // enable anti-aliased rendering (prettier lines and circles)
        // Comment it out to see what this does!
        g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
        g.setColor(Color.BLACK)
        this.render(g, size._1, size._2)

        // done with drawing
        g.dispose()

        // write image to a file
        javax.imageio.ImageIO.write(canvas, "png", new java.io.File(filename))
    }
}
