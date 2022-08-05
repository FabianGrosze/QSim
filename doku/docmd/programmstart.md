Programmstart  {#lnk_programmstart}
==============

Nach Abschluss der \ref lnk_modellerstellung kann das Programm QSim3D gestartet 
werden. Dazu gibt es die folgenden Möglichkeiten:

# Linux-Konsole {#lnk_programmstart_konsole}

## Seriell {#lnk_programmstart_seriell}
Nachdem der Sourcecode von Qsim-3D kompiliert wurde und das daraus entstandene 
ausführbare Programm (Executable) z.B. in der Datei /home/Wyrwa/bin/qsim3d 
abgelegt wurde; kann es dann von der Kommandozeile einer Konsole eines 
Linux-Rechners aus direkt ausgeführt werden.

Z.B. auf dem voss-mod01 mit dem Kommando:

\code wyrwa@voss-mod01:~> /home/Wyrwa/bin/qsim3d /srv/wyrwa/annu/ 769 \endcode
Der erste, obligatorische  Parameter ist das Modellverzeichnis
(mit abschließendem betriebssystemspezifischem Verzeichnistrenner).

Der zweite, optionale Parameter ist eine Kontrollknotennummer für eine 
erweiterte Bildschirmausgabe.

Das Programm wird damit seriell, d. h. alle Operationen werden nacheinander auf 
einem Prozessor ausgeführt.

## Parallel {#lnk_programmstart_parallel}

QSim3D ist mit dem Message Passing Interface (MPI) parallelisiert, um auf 
Parallelrechnern mit verteiltem Speicher ausführbar zu sein;
siehe: \ref lnk_parallelisierung. Es wird dann das Kommando mpiexec verwendet, 
um QSim3d auf einer Gruppe von Prozessoren zu starten.

## Batch-queue {#lnk_batch-queue}

Teure Hochleistungsrechner wie der HPC (high-performance-cluster) der BfG 
stehen in der Regel nicht einzelnen Kollegen alleine zur Verfügung.
Um die Berechnungsaufträge von vielen Benutzern abzuarbeiten, werden sogenannte 
batch-queues (elektronische Warteschlangen) verwendet.

Der Benutzer übergibt seinen Berechnungsauftrag durch Start eines Skriptes an 
einen Scheduler; momentan wird slurm verwendet früher torque.

Beispiel <a href="./exp/qsim3d.sbatch" target="_blank">qsim3d.sbatch</a>\n
Der Auftrag wird dann abgearbeitet, sobald die angeforderten Kapazitäten auf 
dem HPC verfügbar sind.
Sowohl QSim3D als auch slurm verfügen über Möglichkeiten den Benutzer über das 
Berechnungsende per Email zu informieren.

## Fortschritt {#lnk_sim_fortschritt}

Während des Rechenlaufs wird im Modellverzeichnis eine Datei \p fortschritt 
abgelegt, die nur eine Zahl zwischen 0 und 1 enthält, und angibt, welcher Anteil 
der erforderlichen Zeitschritte bereits durchlaufen wurde.
Beim Beenden von QSim-3D wird diese Datei gelöscht. Das Vorhandensein von 
\p fortschritt verhindert, dass weitere
Rechenläufe gestartet werden, die dasselbe Modellverzeichnis verwenden. 

Bei unkontrollierten Programmabbrüchen wird \p fortschritt nicht gelöscht. 
Bei einem Neustart würde dies das Anlaufen von QSim3d verhindern;
d.h. \p fortschritt muss nach unkontrollierten Abbrüchen vor dem Neustart 
manuell entfernt werden.

# Benutzeroberfläche Gerris {#lnk_programmstart_gui}

Wenn bei der \ref lnk_gerris die Optionen richtig gesetzt wurden, läßt sich QSim3D 
durch Drücken auf den "Berechnen" Knopf starten:

\image html GERRISstart.png

Textquelle: programmstart.md ; Codesources:  QSim3D.f95 ;  
zurück: \ref lnk_modellbedienung oder \ref index
 