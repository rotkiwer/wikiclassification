 /**
  MIT LICENSE
  
  Copyright (C) 2012 by Wiktor Wojtylak

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
*/

  import scala.xml.XML
  import scala.collection.immutable
  import scala.collection.mutable
  import java.io.File
  import java.io.InputStream
  import java.io.FileInputStream
  import java.io.IOException
  import java.io.FileOutputStream
  import java.io.ObjectOutputStream
  import java.io.ObjectInputStream  
  import opennlp.tools.tokenize.TokenizerModel
  import opennlp.tools.tokenize.TokenizerME
  import org.jsoup.Jsoup

  case class CategoriesTree(name: String, var subcategories: List[CategoriesTree])

  object WikiClassification extends Application {

    // 1. parsing

    // Returns sorted list of words with frequencies.
    // Filters tokens which are not words, filters most common words.
    def buildWordsFrequencyList(tokens: List[String]): List[(String, Int)] = {

      def filterOnlyWords(tokens: List[String]): List[String] = tokens.filter(t => t.matches("[a-zA-Z]+")) //[a-zA-Z_0-9]+

      def filterCommonWords(tokens: List[String]): List[String] = {
        // The First 100 Most Commonly Used English Words
        // Source: The Reading Teachers Book of Lists, Third Edition; by Edward Bernard Fry, Ph.D, Jacqueline E. Kress, Ed.D & Dona Lee Fountoukidis, Ed.D.
        val mostCommonWords = List("the", "of", "and", "a", "to", "in", "is", "you", "that", "it", "he", "was", "for", "on", "are", "as", "with", "his", "they", "i",
          "at", "be", "this", "have", "from", "or", "one", "had", "by", "word", "but", "not", "what", "all", "were", "we", "when", "your", "can", "said",
          "there", "use", "an", "each", "which", "she", "do", "how", "their", "if", "will", "up", "other", "about", "out", "many", "then", "them", "these", "so",
          "some", "her", "would", "make", "like", "him", "into", "time", "has", "look", "two", "more", "write", "go", "see", "number", "no", "way", "could", "people",
          "my", "than", "first", "water", "been", "call", "who", "oil", "its", "now", "find", "long", "down", "day", "did", "get", "come", "made", "may", "part")
          
        tokens.filterNot(t => mostCommonWords.contains(t))
      }

      def countWords(words: List[String]): List[(String, Int)] = words.groupBy(t => t).mapValues(t => t.length).toList

      countWords(filterCommonWords(filterOnlyWords(tokens))).sort((a, b) => (a._2 > b._2))
    }

    // Transforms String into tokens List (uses Apache OpenNLP Maximum Entropy)
    def tokenize(text: String): List[String] = {
      val modelEn: InputStream = new FileInputStream("lib/opennlp-en-token.bin")
      try {
        val tokenizer = new TokenizerME(new TokenizerModel(modelEn))
        return tokenizer.tokenize(text).toList
      }
      finally {
        if (modelEn != null) {
          try modelEn.close()
        }
      }

    }

    // Erases HTML tags from String.
    def stripHTML(html: String): String = {
      //html.replaceAll("\\<.*?>", "")
      Jsoup.parse(html).text();
    }

    // 2. Categories tree (Wikipedia communication)  
    
    // Recursive function to build Wikipedia Categories tree.
    // Parameters:
    //		tree	- starting structure. For initialization e.g.: buildCategoriesTreeStartingWith(CategoriesTree("Europe", List()), 3)
    //		depth	- how deep should be the tree
    def buildCategoriesTreeStartingWith(tree: CategoriesTree, depth: Int): CategoriesTree = {

      def getURL(url: String): String = io.Source.fromURL(url).mkString
      def prepareURL(s: String): String = s.replaceAll(" ", "%20").replaceAll("&", "%26")

      val WIKIPEDIA_API_URL = "http://en.wikipedia.org/w/api.php"
      val CATEGORY_TAG = "cl"
      val CATEGORY_ATTR = "title"
      val DISAMB_PAGES = "Category:Disambiguation pages"
      val CLIMIT = 500 // how many categories will be taken, 500 is max (limited by MediaWiki API)

      if (depth > 0) {
        val xmlFormat = XML.loadString(getURL(WIKIPEDIA_API_URL + "?action=query&prop=categories&titles=" + prepareURL(tree.name) + "&cllimit=" + CLIMIT + "&clshow=!hidden&format=xml"))
        (xmlFormat \\ CATEGORY_TAG) foreach { categoryNode =>
          val category = (categoryNode \\ ("@" + CATEGORY_ATTR)).toString()
          if (category != DISAMB_PAGES) {
            var subtree = CategoriesTree(category, List())
            tree.subcategories = subtree :: tree.subcategories
            buildCategoriesTreeStartingWith(subtree, depth - 1)
          }
        }
        tree.subcategories = tree.subcategories.reverse // keep alphabetic order
      }
      return tree
    }    
    
    // Transforms tree into flat immutable set structure.
    def convertCategoriesTreeToSet(tree: CategoriesTree): Set[String] = {
      def addValueToSet(tree: CategoriesTree, set: mutable.HashSet[String] = new mutable.HashSet[String]): mutable.Set[String] = {
        set += tree.name
        tree.subcategories foreach { subtree => addValueToSet(subtree, set)}
        set
      }
      addValueToSet(tree).toSet // immutable
    }    
    
    // Builds one big flat categories set for all words.
    // First it builds trees for each word than transforms them into sets. Finally returns a merge from all those sets.
    def buildCategoriesSetFromWordsFrequencyList(wordsFrequencyList: List[(String, Int)], categoriesTreeDepth: Int): Set[String] = {
      val categoriesSet = new mutable.HashSet[String]     
      wordsFrequencyList foreach { w =>
        val tree = buildCategoriesTreeStartingWith(CategoriesTree(w._1.capitalize, List()), categoriesTreeDepth)
        categoriesSet ++= convertCategoriesTreeToSet(tree)
      }      
      categoriesSet.toSet // immutable
    }
    
    def printCategoriesTree(tree: CategoriesTree, indent: Int = 0) {
      for (i <- 1 to indent) print(" ")
      println(tree.name)
      tree.subcategories foreach (subcategory => printCategoriesTree(subcategory, indent + 1))
    }
    
    def log(msg: String) {
      println(msg)
    }
    
    override def main(args: Array[String]) = {
      
      log("Classification started...")
      
      // parameters for program
      val param_groupsMainDir = args(0)
      val param_documentsDir = args(1)
      val param_numberOfMostCommonWordsToAnalyze = args(2).toInt
      val param_categoriesTreeDepth = args(3).toInt
      val param_printIntersection = if(args.length > 4) args(4) else ""
      val serializedGroupsCategoriesFileName = "groups_categories.serialized"

      // serialize groups categories for efficiency (when classification over the same groups occurs many times)
      lazy val groupsToCategoriesMap = getGroupsCategoriesMap(serializedGroupsCategoriesFileName)
      lazy val documentsToCategoriesMap = initDocumentsCategoriesMap()
      // isEmpty to load variables; due to "forward reference extends over definition of value" error
      groupsToCategoriesMap.isEmpty; documentsToCategoriesMap.isEmpty
            
      log("\nClassification results:")
      documentsToCategoriesMap foreach { fileSet =>
        val documentName = fileSet._1
        val groupName = findGroupForDocument(groupsToCategoriesMap, documentsToCategoriesMap, documentName)
      	log(documentName.splitAt(documentName.indexOf("_"))._1 + ";" + groupName)
      }
            
      // --- helpers:         
      
      def findGroupForDocument(groupsToCategoriesMap: Map[String, Set[String]], filesToCategoriesMap: Map[String, Set[String]], documentName: String): String = {
        var maxIntersection = immutable.Set[String]()
        var groupName = ""
        groupsToCategoriesMap foreach { categorySet =>
          val currentIntersection = categorySet._2.intersect(filesToCategoriesMap(documentName))
           if (currentIntersection.size > maxIntersection.size) {
             maxIntersection = currentIntersection
             groupName = categorySet._1
           }
        }
        if (param_printIntersection == "i") {
          log("    Intersection: ")
          maxIntersection foreach (i => log("      " + i))
        }
        groupName
      }
      
      def getGroupsCategoriesMap(serializedFileName: String): Map[String, Set[String]] = {
        val serializedGroupsCategoriesFileExists = new File(serializedGroupsCategoriesFileName).exists() 
        val map =
          if (serializedGroupsCategoriesFileExists) {
            log("\nUsing categories sets for groups from serialized file (" + serializedFileName + ")")
            deserializeGroupsCategoriesMap(serializedGroupsCategoriesFileName)
          }
          else {
            initGroupsCategoriesMap()
          } 
        if (!serializedGroupsCategoriesFileExists) serializeGroupsCategoriesMap(map, serializedGroupsCategoriesFileName)
        map
      }
      
      // init sets for each group (i.e. for each document in each group and merge in groups)
      def initGroupsCategoriesMap(): Map[String, Set[String]] = {
        val groupsToCategoriesMap = new mutable.HashMap[String, Set[String]]
        log("\nInitializing categories sets for groups")
        val groupsSubDirs = new File(param_groupsMainDir).listFiles.filter(f => !f.isHidden() && f.isDirectory())
        for (groupDir <- groupsSubDirs) {
          log("Initializing categories set for group: " + groupDir.getName())
          val categoriesSet = new mutable.HashSet[String]
          for (groupFile <- groupDir.listFiles.filter(f => f.isFile() && !f.isHidden())) {
        	  categoriesSet ++= buildCategoriesSetForFile(groupFile.getPath())
          }
          groupsToCategoriesMap(groupDir.getName()) = categoriesSet.toSet
        }
        groupsToCategoriesMap.toMap
      }
      
      // init set for each document
      def initDocumentsCategoriesMap(): Map[String, Set[String]] = {
        val documentsToCategoriesMap = new mutable.HashMap[String, Set[String]]
        log("\nInitializing categories sets for documents")
        for (documentFile <- new File(param_documentsDir).listFiles.filter(f => !f.isHidden() && f.isFile())) { 
          documentsToCategoriesMap(documentFile.getName()) = buildCategoriesSetForFile(documentFile.getPath())
        }
        documentsToCategoriesMap.toMap
      }
      
      def buildCategoriesSetForFile(filePath: String): Set[String] = {
        log("  file: " + filePath + "...")
        val source = io.Source.fromFile(filePath)
        val lines = source.mkString; source.close   
        val wordsFrequencyList = buildWordsFrequencyList(tokenize(stripHTML(lines.toLowerCase))).take(param_numberOfMostCommonWordsToAnalyze)
        buildCategoriesSetFromWordsFrequencyList(wordsFrequencyList, param_categoriesTreeDepth)
      }       
      
      def serializeGroupsCategoriesMap(map: Map[String, Set[String]], fileName: String) {
        val fos = new FileOutputStream(fileName);
        val out = new ObjectOutputStream(fos);
        out.writeObject(map);
        out.close();
      }
      
      def deserializeGroupsCategoriesMap(fileName: String): Map[String, Set[String]] = {
        val fis = new FileInputStream(fileName);
        val in = new ObjectInputStream(fis);
        val map = in.readObject().asInstanceOf[Map[String, Set[String]]]
        in.close();
        map
      }        
      
    }

  }