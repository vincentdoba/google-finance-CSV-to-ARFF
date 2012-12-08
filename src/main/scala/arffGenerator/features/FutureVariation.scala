package arffGenerator.features

import arffGenerator.data.Data
import scala.collection.mutable.Map

/**
 * User: Vincent DOBA
 * Date: 9/27/11
 * Time: 4:06 PM
 */

object OpeningVariation extends Numeric {
  override def name:String = "openingVariation"

  override def computeList(data:Data):List[String] = {
    def compute(values:List[Map[String, Double]], result:List[String], buffer:Double):List[String] = values match {
      case Nil => result.reverse
      case elem1::l if (buffer==0.0) => compute(l, "?"::result, elem1("close"))
      case elem1::l => compute(l, (elem1("open")-buffer).toString::result, elem1("close"))
    }

    compute(data.samples, Nil, 0.0)
  }
}

object ClosingVariation extends Numeric {
  override def name:String = "closingVariation"

  override def computeList(data:Data):List[String] = {
    def compute(values:List[Map[String, Double]], result:List[String], buffer:Double):List[String] = values match {
      case Nil => result.reverse
      case elem1::l if (buffer==0.0) => compute(l, "?"::result, elem1("close"))
      case elem1::l => compute(l, (elem1("close")-buffer).toString::result, elem1("close"))
    }

    compute(data.samples, Nil, 0.0)
  }
}

object ClosingHigher extends Feature {
  override def name:String = "closingHigher"
  override def nature:String = "{true, false}"

  override def computeList(data:Data):List[String] = {
    def compute(values:List[Map[String, Double]], result:List[String], buffer:Double):List[String] = values match {
      case Nil => result.reverse
      case elem1::l if (buffer==0.0) => compute(l, "?"::result, elem1("close"))
      case elem1::l if (buffer>elem1("close")) => compute(l, "true"::result, elem1("close"))
      case elem1::l => compute(l, "false"::result, elem1("close"))
    }

    compute(data.samples, Nil, 0.0)
  }
}

object OpeningHigher extends Feature {
  override def name:String = "closingHigher"
  override def nature:String = "{true, false}"

  override def computeList(data:Data):List[String] = {
    def compute(values:List[Map[String, Double]], result:List[String], buffer:Double):List[String] = values match {
      case Nil => result.reverse
      case elem1::l if (buffer==0.0) => compute(l, "?"::result, elem1("close"))
      case elem1::l if (buffer>elem1("open")) => compute(l, "true"::result, elem1("close"))
      case elem1::l => compute(l, "false"::result, elem1("close"))
    }

    compute(data.samples, Nil, 0.0)
  }
}

object ChangeVariation extends Feature {
  override def name:String = "changeVariation"
  override def nature:String = "{true, false}"

  override def computeList(data:Data):List[String] = {
    def compute(values:List[Map[String, Double]], result:List[String], buffer:Double):List[String] = values match {
      case Nil => result.reverse
      case elem1::Nil => ("?"::result).reverse
      case elem1::elem2::l if (buffer==0.0) => compute(elem2::l, "?"::result, elem1("close")-elem2("close"))
      case elem1::elem2::l if (buffer*(elem1("close")-elem2("close")) < 0) => {
        compute(elem2::l, "true"::result, elem1("close")-elem2("close"))
      }
      case elem1::elem2::l => compute(elem2::l, "false"::result, elem1("close")-elem2("close"))
    }

    compute(data.samples, Nil, 0.0)
  }
}