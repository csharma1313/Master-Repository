/*
 *  Power BI Visual CLI
 *
 *  Copyright (c) Microsoft Corporation
 *  All rights reserved.
 *  MIT License
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the ""Software""), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED *AS IS*, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 */

module powerbi.extensibility.visual {
    interface BarChartViewModel{  //interface for the visual model
        dataPoints: BarChartDataPoint[];
        dataMax: number;
        settings : BarChartSettings;
    }

    interface BarChartSettings{
        enableAxis: {
            show: boolean;
        }
    }

    interface BarChartDataPoint{   //interface for individual data point
        value: PrimitiveValue; //simple value
        category: string;
        color: string;
        selectionID: ISelectionId;  //to select individual bars
    }

    function getOptionValue<T>(objects: DataViewObject, objectName: string, propertyName: string, defaultValue: T): T {
        if(objects){
            let object = objects[objectName];
            if(object){
                let property : T = <T> object[propertyName];
                if(property != undefined) {
                    return property;
                }
            }

        }
        return defaultValue;
    }

    function visualTransform(options: VisualUpdateOptions, host: IVisualHost) : BarChartViewModel {
        let dataViews = options.dataViews;
        
        let defaultSettings: BarChartSettings ={
            enableAxis: {
                show: false
            }
        }

        let dataInfo : BarChartViewModel = {
            dataPoints: [],
            dataMax: 0,
            settings: defaultSettings
        };

        if (!dataViews
            || !dataViews[0]
            || !dataViews[0].categorical
            || !dataViews[0].categorical.categories
            || !dataViews[0].categorical.categories[0].source
            || !dataViews[0].categorical.values)
            return dataInfo;

        let categorical = dataViews[0].categorical;
        let category = categorical.categories[0];
        let dataValue = categorical.values[0];

        let dataPoints: BarChartDataPoint[] = [];
        let dataMax: number;

        let colorPalette: IColorPalette= host.colorPalette; //color palette provided by power bi
        
        let objects = dataViews[0].metadata.objects;
        let barChartSettings : BarChartSettings = {
            enableAxis: {
                show: getOptionValue<boolean>(objects, 'enableAxis', 'show', defaultSettings.enableAxis.show)
            }
        }

        for (let i = 0, len = Math.max(category.values.length, dataValue.values.length); i < len; i++) {
            dataPoints.push({
                category: <string>category.values[i],
                value: dataValue.values[i],
                color: colorPalette.getColor(<string>category.values[i]).value,
                selectionID: host.createSelectionIdBuilder()  //allow power bi to assign ids to selected data points
                .withCategory(category, i) //telling which element we are on right now   
                .createSelectionId()
            });
        }
        dataMax = <number>dataValue.maxLocal;

        return {
            dataPoints: dataPoints,
            dataMax: dataMax,
            settings : barChartSettings
        };
    }

    export class Visual implements IVisual {
        private host: IVisualHost;
        private svg: d3.Selection<SVGElement>;
        private barContainer: d3.Selection<SVGElement>;
        private selectionManager: ISelectionManager; 
        private xAxis: d3.Selection<SVGElement>;
        private barChartSettings : BarChartSettings;

        constructor(options: VisualConstructorOptions) {
            this.host = options.host;
            this.selectionManager= options.host.createSelectionManager();
            this.svg = d3.select(options.element)
                .append('svg')
                .classed('barChart', true);

            this.barContainer = this.svg
                .append('g')
                .classed('barContainer', true);

            this.xAxis= this.svg
            .append('g')
            .classed('xAxis', true);
        }

        public update(options: VisualUpdateOptions) {
            let transformedData: BarChartViewModel = visualTransform(options, this.host);
            this.barChartSettings = transformedData.settings;
            let width = options.viewport.width;
            let height = options.viewport.height;

            this.svg.attr({
                width: width,
                height: height
            });

            if(this.barChartSettings.enableAxis.show) {
                height= height-25  //setting margin
            }

            this.xAxis.style({
                'font-size': d3.min([height, width]) * 0.04          //dynamic sizing. we can also do static by adding poperty in visual.less file
            });

            let yScale = d3.scale.linear()
                .domain([0, transformedData.dataMax])
                .range([height, 0]);

            let xScale = d3.scale.ordinal()
                .domain(transformedData.dataPoints.map(dataPoint => dataPoint.category))
                .rangeRoundBands([0, width], 0.1, 0.2);

            let xAxis = d3.svg.axis()    //setting labels
            .scale(xScale)   //positioning them according to scale
            .orient('bottom'); //where to place

            this.xAxis.attr({'transform': 'translate(0, ' + height + ')' }) //accessing xAxis property
            .call(xAxis) //accessing xAxis var and call applies this configuration to the shape

            let bars = this.barContainer
                .selectAll('.bar')
                .data(transformedData.dataPoints);

            bars.enter()
                .append('rect')
                .classed('bar', true);

            bars.attr({
                width: xScale.rangeBand(),
                height: data => height - yScale(<number>data.value),
                x: data => xScale(data.category),
                y: data => yScale(<number>data.value),
                fill: data=>data.color
            });

            let selectionManager= this.selectionManager;

            bars.on('click', function(dataPoint) {    //click event
                    selectionManager.select(dataPoint.selectionID) //we have selected a data point
                    .then((ids: ISelectionId[])=> {
                       bars.attr ({
                            'fill-opacity': ids.length >0 ? 0.5 :1           //if ids. length is selected then make opacity 50% otherwise 100%
                       })
                       d3.select(this).attr({
                           'fill-opacity':1
                       })
                    })     //get all ids
            });  

            bars.exit().remove();
        }

        public enumerateObjectInstances(options : EnumerateVisualObjectInstancesOptions) : VisualObjectInstanceEnumeration {//to push our settings object to pbi
            let objectName = options.objectName; // executes for each option   
            let objectEnumeration : VisualObjectInstance [] =[]; // basically our array of objects

            switch(objectName){
                case 'enableAxis':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties : {
                            show: this.barChartSettings.enableAxis.show
                        },
                        selector: null
                    });
            }
            return objectEnumeration;
        }
    }
}