package edu.knowitall.tac2013.findSlotFillersApp

import CandidateType._

case class KbpSolrQuery(val queryString: String, val resultType: CandidateType)

class QueryBuilder(val pattern: SlotPattern, val entityName: String, val nodeId: Option[String]) {
  
  private def getQueryString(fields: Seq[String]) = {
    val nonEmptyFields = fields.filter(_.nonEmpty)

    nonEmptyFields.mkString(" AND ")
  }

  def arg1TextConstraint: Option[String] = {
    pattern.entityIn match {
      case Some("arg1") => Some("+arg1Text:\"%s\"".format(entityName))
      case _ => None
    }
  }

  def relTextConstraint: Option[String] = {
    pattern.openIERelationString match {
      case Some(relString) => {
        val noJobTitle = relString.replace("<JobTitle>", "")
        val filteredRel = if (noJobTitle != "") {
          "+relText:\"" + noJobTitle + "\""
        } else {
          ""
        }
        Some(filteredRel)
      }
      case None => None
    }
  }

  def arg2TextConstraint: Option[String] = {
    pattern.entityIn match {
      case Some("arg2") => Some("+arg2Text:\"%s\"".format(entityName))
      case _ => None
    }
  }
  
  def arg2StartConstraint: Option[String] = {
    pattern.arg2Begins match {
      case Some(arg2Begins) => Some("+arg2Text:\"%s\"".format(arg2Begins))
      case None => None
    }
  }
  
  def arg1LinkConstraint: Option[String] = {
    (pattern.entityIn, nodeId) match {
      case (Some("arg1"), Some(id)) => Some("+arg1WikiLinkNodeId:\"%s\"".format(id))
      case _ => None
    }
  }


  def arg2LinkConstraint: Option[String] = {
    (pattern.entityIn, nodeId) match {
      case (Some("arg2"), Some(id)) => Some("+arg2WikiLinkNodeId:\"%s\"".format(id))
      case _ => None
    }
  }
  
  def getQueries: Seq[KbpSolrQuery] = {
    
    Seq(buildQuery) ++ {
      nodeId match {
        case Some(id) => Some(buildLinkedQuery)
        case None => None
      }
    }
  }
  

  def buildQuery: KbpSolrQuery = {
    
    val queryFields = Seq(arg1TextConstraint, arg2TextConstraint, relTextConstraint, arg2StartConstraint).flatten
    
    KbpSolrQuery(getQueryString(queryFields), CandidateType.REGULAR)
  }

  def buildLinkedQuery: KbpSolrQuery = {
    
    val queryFields = Seq(arg1LinkConstraint, arg2LinkConstraint, relTextConstraint, arg2StartConstraint).flatten
    
    KbpSolrQuery(getQueryString(queryFields), CandidateType.LINKED)
  }

}