name := "KnowItAll_TAC2013_SlotFiller"

assemblySettings

net.virtualvoid.sbt.graph.Plugin.graphSettings

version := "0.0.1"

scalaVersion := "2.10.2"

organization := "edu.knowitall"

libraryDependencies += "edu.stanford.nlp" % "stanford-corenlp" % "1.3.4"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "edu.washington.cs.knowitall.nlptools" % "nlptools-chunk-opennlp_2.10" % "2.4.2"

libraryDependencies += "edu.washington.cs.knowitall.nlptools" % "nlptools-parse-clear_2.10" % "2.4.2"

libraryDependencies += "edu.washington.cs.knowitall.srlie" %% "openie-srl" % "1.0.0-RC1"

libraryDependencies += "net.liftweb" %% "lift-json" % "2.5-RC5"

libraryDependencies += "org.apache.solr" % "solr-solrj" % "4.3.0"

libraryDependencies += "edu.washington.cs.knowitall.chunkedextractor" %% "chunkedextractor" % "1.0.4"

javaOptions in run += "-Xmx8G"

fork in run := true