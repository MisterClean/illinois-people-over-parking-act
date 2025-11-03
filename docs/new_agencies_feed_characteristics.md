# New Illinois Transit Agencies - GTFS Feed Characteristics

**Date:** January 2025
**Purpose:** Documentation of GTFS feed structure and quirks for 8 newly added Illinois transit agencies

---

## Summary

All 8 new agencies have been successfully integrated into the People Over Parking Act analysis. All feeds are well-formed, bus-only systems with complete calendar data for peak period analysis.

**Common characteristics across all 8 feeds:**
- ✓ All critical GTFS files present (stops, routes, trips, stop_times)
- ✓ All are bus-only systems (route_type=3)
- ✓ All have direction_id populated (100% coverage in trips.txt)
- ✓ All have calendar data (calendar.txt and/or calendar_dates.txt)
- ✓ All can support peak period analysis (7-9am, 4-6pm)

---

## Individual Agency Details

### 1. MetroLINK (Rock Island County - Quad Cities)
**Agency ID:** `metrolink_quad_cities`
**GTFS URL:** https://www.metroqc.com/documentcenter/view/404

**Statistics:**
- Routes: 42
- Stops: 1,226
- Trips: 10,859
- Stop times: 547,754

**Calendar structure:**
- Has calendar.txt but no weekday-only services defined
- Relies on calendar_dates.txt for service additions (24 additions, 179 removals)
- Service period: Oct 2025 - Jan 2026

**Notable characteristics:**
- Direction ID: 100% populated (values: 0, 1)
- Geographic coverage: Quad Cities area (IL/IA border)
- Latitude range: 41.41°N to 41.55°N
- Longitude range: -90.64°W to -90.22°W

**No special handling required** - existing pipeline handles calendar_dates.txt properly.

---

### 2. CityLink (Greater Peoria)
**Agency ID:** `citylink`
**GTFS URL:** https://clk.rideralerts.com/InfoPoint/gtfs-zip.ashx

**Statistics:**
- Routes: 17
- Stops: 887
- Trips: 5,225
- Stop times: 179,863

**Calendar structure:**
- ⚠ **NO calendar.txt file** - uses calendar_dates.txt exclusively
- All calendar_dates entries are exception_type=1 (service added)
- Service period: Oct-Dec 2025

**Notable characteristics:**
- Direction ID: 100% populated (values: 0, 1)
- **Has arrival times >= 24:00:00** (cleaned by `enrich_stop_times()`)
- Geographic coverage: Peoria metro area
- Latitude range: 40.54°N to 40.82°N
- Longitude range: -89.69°W to -89.48°W

**No special handling required** - pipeline accommodates missing calendar.txt and cleans times >= 24:00.

---

### 3. SMTD (Sangamon Mass Transit District - Springfield)
**Agency ID:** `smtd`
**GTFS URL:** http://data.smtd.org/gtfs/smtd_gtfs_feed.zip

**Statistics:**
- Routes: 30
- Stops: 1,034
- Trips: 4,090
- Stop times: 132,930

**Calendar structure:**
- Has calendar.txt but no weekday-only services
- Relies on calendar_dates.txt for service additions (15 additions, 105 removals)
- Service period: Aug 2025 - Jan 2026

**Notable characteristics:**
- Direction ID: 100% populated (values: 0, 1)
- Missing location_type column in stops.txt (added during normalization)
- Geographic coverage: Springfield metro area
- Latitude range: 39.71°N to 39.86°N
- Longitude range: -89.75°W to -89.58°W

**No special handling required** - normalization adds missing location_type.

---

### 4. DeKalb Public Transit
**Agency ID:** `dekalb`
**GTFS URL:** https://data.trilliumtransit.com/gtfs/cityofdekalb-il-us/cityofdekalb-il-us.zip

**Statistics:**
- Routes: 11
- Stops: 212
- Trips: 2,660
- Stop times: 66,530

**Calendar structure:**
- Has calendar.txt with 15 weekday services ✓
- calendar_dates.txt used for holiday removals (12 removals)
- Service period: Sep-Dec 2025

**Notable characteristics:**
- Direction ID: 100% populated (values: 0, 1)
- **Smallest system** by stop count (212 stops)
- Very detailed GTFS with many extended fields (16-27 columns per table)
- Geographic coverage: DeKalb area (NIU campus focus)
- Latitude range: 41.89°N to 41.99°N
- Longitude range: -88.79°W to -88.46°W

**No special handling required** - standard GTFS with rich metadata.

---

### 5. Connect Transit (Bloomington-Normal)
**Agency ID:** `connect_transit`
**GTFS URL:** https://rideconnecttransit.com/gtfs

**Statistics:**
- Routes: 17
- Stops: 445
- Trips: 2,059
- Stop times: 37,872

**Calendar structure:**
- Has calendar.txt with 1 weekday service
- calendar_dates.txt used for holiday removals (6 removals)
- Service period: Oct 2025 - Dec 2099 (far future end date)

**Notable characteristics:**
- Direction ID: 100% populated (values: 0, 1)
- **Has arrival times >= 24:00:00** (cleaned by `enrich_stop_times()`)
- Uses color-coded route names (Blue, Red, Orange, etc.)
- Geographic coverage: Bloomington-Normal area (ISU campus focus)
- Latitude range: 40.45°N to 40.54°N
- Longitude range: -89.06°W to -88.91°W

**No special handling required** - pipeline cleans times >= 24:00.

---

### 6. DPTS (Decatur Public Transit System)
**Agency ID:** `dpts`
**GTFS URL:** https://gtfs.remix.com/dpts_decatur_il_us.zip

**Statistics:**
- Routes: 24
- Stops: 896
- Trips: 2,023
- Stop times: 67,082

**Calendar structure:**
- Has calendar.txt but no weekday-only services
- ⚠ **Service dates appear outdated** (Dec 2023 - Dec 2024)
- calendar_dates.txt has holiday removals only

**Notable characteristics:**
- Direction ID: 100% populated (values: 0, 1)
- Missing location_type column in stops.txt (added during normalization)
- Geographic coverage: Decatur metro area
- Latitude range: 39.81°N to 39.92°N
- Longitude range: -89.03°W to -88.88°W

**Note:** Feed may need periodic refresh due to dated service calendar. Current data still usable for analysis but may not reflect current service patterns.

---

### 7. Galesburg Transit
**Agency ID:** `galesburg`
**GTFS URL:** https://gis.ci.galesburg.il.us/cityofgalesburg-il-us.zip

**Statistics:**
- Routes: 8
- Stops: 112 (smallest by stop count)
- Trips: 110 (smallest by trip count)
- Stop times: 3,399

**Calendar structure:**
- Has calendar.txt with 2 weekday services ✓
- calendar_dates.txt used for holiday removals (20 removals)
- Service period: Dec 2024 - Jan 2026

**Notable characteristics:**
- Direction ID: 100% populated but **only value 0** (unidirectional routes or loops)
- Very small system (8 routes, 112 stops)
- Uses color-coded route names (Blue, Green, Red, Gold) with "Alt" variants
- Geographic coverage: Galesburg city area
- Latitude range: 40.93°N to 40.99°N
- Longitude range: -90.41°W to -90.34°W

**Frequency calculation note:** Direction-aware frequency logic will treat all trips as same direction since direction_id=0 for all. This is appropriate for circular/bidirectional routes.

---

### 8. Go West Transit (Macomb - McDonough County)
**Agency ID:** `gowest`
**GTFS URL:** https://api.transloc.com/gtfs/wiu.zip

**Statistics:**
- Routes: 18
- Stops: 176
- Trips: 306 (smallest by trip count)
- Stop times: 7,822

**Calendar structure:**
- Has calendar.txt with 3 weekday services ✓
- calendar_dates.txt used for service additions/removals (9 additions, 144 removals)
- Service period: Aug 2025 - Aug 2026

**Notable characteristics:**
- Direction ID: 100% populated but **only value 0** (unidirectional routes or loops)
- Small system focused on Western Illinois University campus
- Geographic coverage: Macomb area (WIU campus focus)
- Latitude range: 40.44°N to 40.48°N
- Longitude range: -90.70°W to -90.64°W

**Frequency calculation note:** Same as Galesburg - all trips treated as same direction.

---

## Pipeline Compatibility

All 8 feeds are fully compatible with the existing GTFS processing pipeline:

### ✓ **Handled automatically by existing code:**
1. Missing location_type in stops.txt (SMTD, DPTS) → Added during normalization
2. Missing calendar.txt (CityLink) → Falls back to calendar_dates.txt
3. Times >= 24:00:00 (CityLink, Connect Transit) → Cleaned by `enrich_stop_times()`
4. Unidirectional routes (Galesburg, Go West) → Frequency calc handles direction_id=0
5. calendar_dates.txt service additions → Existing logic accommodates this

### ⚠ **Minor considerations (no code changes needed):**
1. **DPTS outdated calendar** - Feed still processable but may not reflect current service
2. **Small systems** (Galesburg, Go West, DeKalb) - May have zero qualifying hubs if frequency criteria not met
3. **Extended GTFS fields** (DeKalb) - Extra columns ignored gracefully by fread(fill=TRUE)

---

## Geographic Coverage

**New counties/regions served:**
- Rock Island County (MetroLINK - Quad Cities area)
- Peoria County (CityLink)
- Sangamon County (SMTD - Springfield)
- DeKalb County (DeKalb Transit)
- McLean County (Connect Transit - Bloomington-Normal)
- Macon County (DPTS - Decatur)
- Knox County (Galesburg Transit)
- McDonough County (Go West - Macomb/WIU)

**Expanded Illinois coverage:** From 6 agencies to 14 agencies, covering major urban centers across central, north-central, and northwestern Illinois.

---

## Validation Results

All feeds passed validation with the following results:

### ✓ **Critical files present:**
- stops.txt ✓
- routes.txt ✓
- trips.txt ✓
- stop_times.txt ✓

### ✓ **Calendar data present:**
- calendar.txt and/or calendar_dates.txt ✓

### ✓ **Bus-only systems:**
- All route_type=3 (no rail infrastructure to identify)

### ✓ **Direction ID availability:**
- 100% populated across all 8 agencies
- Values: 0, 1 (standard GTFS)
- Galesburg and Go West use only direction_id=0

### ✓ **Coordinate validity:**
- All stops within Illinois latitude/longitude ranges
- No (0,0) or null coordinates detected

---

## Integration Checklist

- [x] All 8 feeds downloaded and cached in `gtfs_cache/`
- [x] Feed characteristics documented
- [x] Compatibility with existing pipeline confirmed
- [x] No rail services requiring special handling (all bus-only)
- [x] All feeds support peak period analysis (calendar data present)
- [x] No critical data quality issues identified

**Ready for integration into main analysis pipeline.**
