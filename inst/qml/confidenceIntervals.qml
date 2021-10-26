//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//


import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0


Form 
{

	Group
	{
		DoubleField  { name: "mu";	label: qsTr("μ"); negativeValues: true	}
		DoubleField { name: "sigma";	label: qsTr("σ");	negativeValues: false;	max: 100;	defaultValue: 1	}
		IntegerField { name: "n";	label: qsTr("n");	min: 2;	max: 10000; defaultValue: 10				}
		CIField { name: "confidenceIntervalInterval"; label: qsTr("Confidence level") }
		IntegerField { name: "nReps";	label: qsTr("Repetitions");	min: 1;	max: 10000;	defaultValue: 10				}
    }

    Divider { }

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "treePlot";		label: qsTr("Tree plot"); checked: true
			CheckBox
			{
				name: "tableTreePlot"; label: qsTr("Display table"); checked: false
			}
			CheckBox 
			{ 
				name: "fixAxisTreePlot"; label: qsTr("Fix x-axis from "); checked: false;	childrenOnSameRow: true
				 		DoubleField  { name: "fixAxisLower";	label: qsTr(""); negativeValues: true	
				 		defaultValue: -2; afterLabel: qsTr(" to ")}
		 				DoubleField  { name: "fixAxisUpper";	label: qsTr(""); negativeValues: true
		 				defaultValue: 2	}
			}
		}
		CheckBox
		{
			name: "dataPlot";	label: qsTr("Data plots")
			IntegerField { name: "dataPlotShowN";	label: qsTr("Show last");	min: 1;	max: 10;	defaultValue: 1				}
		}
	}

}

