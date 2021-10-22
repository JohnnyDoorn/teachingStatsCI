import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
    title: 		"Learning Stats"
    description: 	"This is a totally amazing module."
    version: 		"0.0.1"
    author: 		"Johnny"
    maintainer: 	"yourName"
    website: 		"yourName.org"
    license: 		"GPL (>= 2)"
    icon:	        "LearnStats.svg"
  

  Analysis
  {
  	title:	"Confidence Intervals"
  	qml:	"confidenceIntervals.qml"
  	func: 	"ConfidenceIntervals"
	requiresData:	false

  }

}
