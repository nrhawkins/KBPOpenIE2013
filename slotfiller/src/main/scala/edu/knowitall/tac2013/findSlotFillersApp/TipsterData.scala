package edu.knowitall.tac2013.findSlotFillersApp

object TipsterData {
  
  private val tipsterPath = "/homes/gws/jgilme1/docs/Tac2013/TipsterGazetteer.txt"
    
  val citySet =  scala.collection.mutable.Set[String]()
  val stateOrProvinceSet = scala.collection.mutable.Set[String]()
  val countrySet = scala.collection.mutable.Set[String]()

  
  scala.io.Source.fromFile(tipsterPath)(scala.io.Codec.ISO8859).getLines.foreach(line => {
      val pairs = line.split("\\)")
      val pairSplits = { for(p <- pairs) yield p.split("\\(")}
      for(nameAndLocationType <- pairSplits){
        if(nameAndLocationType.size ==2){
	        val name = nameAndLocationType(0).trim().toLowerCase()
	        val locationType = nameAndLocationType(1).split(" ")(0).trim()	        
	        locationType match {
	          case "CITY" => { if(!citySet.contains(name)){
	        	  					citySet.add(name)
	        	  					if(name == "redmond"){
	        	  					  println("Redmond added!")
	        	  					}
	          }
	          }
	          case "COUNTRY" => { if(!countrySet.contains(name)) {
	        	  					countrySet.add(name)
	        	  					}
	          }
	          case "PROVINCE" => { if(!stateOrProvinceSet.contains(name)){
	        	  				stateOrProvinceSet.add(name)
	          					}
	          }
	          case _ => {}
	        }
        }
      }
      
      
    })
  
  
  lazy val cities = citySet.toSet
  lazy val countries = countrySet.toSet
  lazy val stateOrProvinces = stateOrProvinceSet.toSet
  
}