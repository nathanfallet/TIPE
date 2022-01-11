//
//  ViewModel.swift
//  HealthDataExtraction
//
//  Created by Nathan Fallet on 11/01/2022.
//

import Foundation
import HealthKit

class ViewModel: ObservableObject {
    
    @Published var startDate: Date = Date() {
        didSet {
            summaryQuery()
        }
    }
    @Published var endDate: Date = Date() {
        didSet {
            summaryQuery()
        }
    }
    
    @Published var data: String = ""
    
    func onAppear() {
        askForAuthorization()
    }
    
    func askForAuthorization() {
        // Get store
        let store = HKHealthStore()
        
        // Ask for authorization
        store.requestAuthorization(
            toShare: nil,
            read: Set([
                HKObjectType.activitySummaryType(),
                HKObjectType.quantityType(forIdentifier: .activeEnergyBurned)!,
                HKObjectType.quantityType(forIdentifier: .appleExerciseTime)!,
                HKObjectType.categoryType(forIdentifier: .appleStandHour)!
            ])
        ) { success, error in
            self.summaryQuery()
        }
    }
    
    func summaryQuery() {
        // Create store
        let store = HKHealthStore()
        
        // Create the predicate
        let calendar = NSCalendar.current
        
        let units: Set<Calendar.Component> = [.day, .month, .year, .era]
        
        var startDateComponents = calendar.dateComponents(units, from: startDate)
        startDateComponents.calendar = calendar
        
        var endDateComponents = calendar.dateComponents(units, from: endDate)
        endDateComponents.calendar = calendar
        
        let summariesWithinRange = HKQuery.predicate(
            forActivitySummariesBetweenStart: startDateComponents,
            end: endDateComponents
        )
        
        // Create the query
        let query = HKActivitySummaryQuery(predicate: summariesWithinRange) {
            query, summaries, error in
            // Check that data is available
            guard let summaries = summaries, !summaries.isEmpty else {
                return
            }
            
            // Convert to entries
            let entries = summaries.map { summary -> Entry? in
                if let day = summary.dateComponents(for: calendar).day,
                    let month = summary.dateComponents(for: calendar).month,
                    let year = summary.dateComponents(for: calendar).year {
                    return Entry(
                        date: String(format: "%04d-%02d-%02d", year, month, day),
                        move: Int(summary.activeEnergyBurned.doubleValue(for: .kilocalorie())),
                        exercise: Int(summary.appleExerciseTime.doubleValue(for: .minute())),
                        stand: Int(summary.appleStandHours.doubleValue(for: .count()))
                    )
                }
                
                return nil
            }
            
            // Do something with entries
            DispatchQueue.main.async {
                let encoder = JSONEncoder()
                encoder.outputFormatting = .prettyPrinted
                
                guard let raw = try? encoder.encode(entries) else {
                    return
                }
                
                self.data = String(data: raw, encoding: .utf8) ?? ""
            }
        }
        
        // Execute the query
        store.execute(query)
    }
    
}
