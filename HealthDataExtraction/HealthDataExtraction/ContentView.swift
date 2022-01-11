//
//  ContentView.swift
//  HealthDataExtraction
//
//  Created by Nathan Fallet on 11/01/2022.
//

import SwiftUI

struct ContentView: View {
    
    @StateObject var viewModel = ViewModel()
    
    var body: some View {
        NavigationView {
            VStack {
                DatePicker("Date de début des données", selection: $viewModel.startDate, displayedComponents: .date)
                DatePicker("Date de fin des données", selection: $viewModel.endDate, displayedComponents: .date)
                TextEditor(text: $viewModel.data)
            }
            .padding()
            .navigationTitle("HealthDataExtraction")
        }
        .onAppear(perform: viewModel.onAppear)
    }
    
}

struct ContentView_Previews: PreviewProvider {
    
    static var previews: some View {
        ContentView()
    }
    
}
