Erosion {#lnk_erosion}
================================

Unter Erosion wird hier das Herauslösen von Feststoffpartikeln aus der Flusssohle und der Sohle in Buhnenfeldern 
infolge der auf sie wirkenden Strömungskräfte verstanden.

Maßgebend für den Massenstrom bei Erosion ist der Schubspannungsüberschuss, 
welcher aus der Differenz zwischen der an der Sohle wirksamen und 
der für den Transportbeginn erforderlichen (kritischen) Sohlschubspannung resultiert.
Zur Beschreibung der Massenerosion konsolidierter Sedimente mit kohäsiven Eigenschaften 
hat sich die folgende Erosionsformel von KERN (1997) bewährt:

\f$ E=M* \left( \frac{ \tau_{0}-\tau_{0,krit} }{ \tau_{0,krit} } \right)^n \f$ für \f$\tau_{0} > \tau_{0,krit} \f$

und

\f$ E=0 \f$ für \f$ \tau_{0} ≤ \tau_{0,krit} \f$

mit:
| Formelzeichen | Bedeutung | Einheit |
| ------ | --------| -------|
| \f$ E \f$ | Erosionsrate |  [(kg/m²)/s]| 
| \f$ M \f$ | Erosionsbeiwert |  [(kg/m²)/s] |
| \f$ \tau_{0} \f$  | Sohlschubspannung |  [kg/(m*s²)] |
| \f$ \tau_{0,krit} \f$  | kritische Erosionsschubspannung |  [kg/(m*s²)] |
| \f$ n \f$  | empirischer Exponent |  [-] |

Erosionsbeiwert, kritische Erosionsschubspannung und empirischer Exponent können Strang-Abschnittsweise (1D) oder Zonen-weise (3D) 
von der Anwenderin vorgegeben werden.

\warning Das von der Erosion resuspendierte Material enthält keine zehrungsfähigen organischen Kohlenstoffe (\ref lnk_orgC) 
und auch keine lebenden Organismen. Obwohl diese in QSim sedimentieren (\ref lnk_sedimentation) können.

Als Rückgabewerte:
SSeros=E*dt/tiefe - Schwebstoffkonzentrationsänderung je Zeitschritt
dsedh=1000.0 * E*dt/sed_roh - Sohlhöhenänderung im aktuellen Zeitschritt [mm]

mit sed_roh wird die Packungs-Dichte des liegenden Sediments in kg Sediment pro Kubikmeter liegendem Bodenvolumen 
Strang-Abschnittsweise (1D) oder Zonen-weise (3D) vorgegeben. ( MODELLG.3D Zeile E )

\warning Die vom Erosionsmodul berechnete Sohlhöhenänderung wirkt nicht auf die Querprofile/Bathymetrie zurück, mit der die hydraulische Simulation durchgeführt wurde. Sie dient nur der Information, und sollte beachtet werden, um entscheiden zu können, ob der Effekt in die Hydraulik mit einfließen sollte.

Bilanz der Schwebstoffkonzentrationen schon in in Erosion nicht in Schwebstoff:
ssalg = ssalg_s + SSeros_s*1000. , ss =

Erläuterung sedroh in Gerris ????????????????????????????????ß

Textquelle: erosion.md ; Codesources: erosion.f90;
zurück: \ref index

---------------------------------------
      dRero_s = m_eros_s*((tau_s-tausc_s)/tausc_s)**n_eros_s
      dRero_s = dRero_s * tflie * 86400.
      dsedh_s = 1000.0 * dRero_s/sedroh_s
      SSeros_s = (dRero_s/tiefe_s)
      ss_s = ss_s + SSeros_s*1000.
      ssalg_s = ssalg_s + SSeros_s*1000.

