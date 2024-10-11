# UML Profile Creation Using Papyrus for CARiSMA

## Prerequisites

- **Eclipse Modeling Tools**: Version 2024-03 (4.31.0)
- **Papyrus**: Version 6.6.0.202401120925
- Ensure you are working on the workspace that contains the `carisma-tool` project.

---

## 1. Create a Papyrus Profile Project

1. Go to `File > New > Project > Papyrus Project`.
2. Under **Software Engineering**, select `Profile`.
3. Name the project: `carisma.profile.<profilename>`.
    - Model File Name: `<profilename>`
    - Root Model Element Name: `<profilename>`
    - Representation Kind: `Profile Diagram`
4. Click `Finish`.
5. Open `<profilename>.profile.di`.
6. Select `Create View`.
    - Double-click on the element with the name `<profilename>` and select `Profile Diagram`.
7. In the **Profile Diagram**, make necessary extensions:
    - To add a new stereotype to an **ActivityPartition** element in an Activity Diagram:
      - Drag and drop `Import Metaclass` from the **Nodes** palette.
      - Search and select `ActivityPartition`.
      - Drag and drop `Stereotype` from the **Nodes** palette and name it (e.g., `test`).
      - Use `Extension` from the **Edges** palette to connect the stereotype to the metaclass.
    - To add tag values:
      - Click on the stereotype (e.g., `test`), navigate to the **Properties** tab.
      - Select `UML`, then under **Owned Attribute**, add attributes (name, label, type, multiplicity, etc.).
8. Save the profile and provide dynamic definition details (date, author, version). Update the dynamic definition with every change.

---

## 2. Create Ecore Model from the Profile

1. Right-click on the UML file under `<profilename>.profile` and select `New > Other > Eclipse Modeling Framework > EMF Generator Model`.
2. Set the parent folder and file name: `<profilename>.profile.genmodel`.
3. Select `UML Model` as the Model Importer and click `Next`.
4. Ensure the Model URI is correct, click `Load`, and then `Next`.
5. Under **Root Packages**, select `<profilename>` package and set the file name to `<profilename>.profile.ecore`.
6. Under **Referenced generator models**, select `Ecore`, `Types`, and `UML` (if present).
7. Click `Finish`.
8. Right-click the generated `.genmodel` file, open it with `EMF Generator`, right-click the root package `<profilename>`, and select `Generate Model Code`.
9. Refactor the generated `src` package to `gen-src` and rename packages to `carisma.profile.<profilename>`.
10. Create a new `src` package and set it up as a source folder in the Java Build Path.

---

## 3. Integrate the New Profile in CARiSMA

1. Under `src`, create:
    - `Activator.java`
    - `<ProfileName>.java`
    - `<ProfileName>Util.java`
    - Refer to a similar structure from the CARiSMA GitHub page.
2. Open `MANIFEST.MF` in the `META-INF` folder and set up:
    - **Overview Tab**: Set `ID`, `Version`, and select `Activator.java` under Activator.
    - **Dependency Tab**: Add required plug-ins:
      - `org.eclipse.core.runtime`
      - `org.eclipse.emf.ecore`
      - `org.eclipse.uml2.types`
      - `org.eclipse.uml2.uml`
      - `carisma.core`
      - `carisma.modeltype.uml2`
      - `org.eclipse.papyrus.uml.extensionpoints`
    - **Runtime Tab**: Add packages in `gen-src` under Exported Packages.
    - **Extensions Tab**:
        - `org.eclipse.uml2.uml.generated_package`
            - Right-click and select `New > profile`.
            - URI: `http://www.umlsec.de/profiles/UMLsec/<profilename>`
            - Location: `platform:/plugin/carisma.profile.umlsec.<profilename>/profile/<profilename>.profile.uml#<xmi_id>`
        - `org.eclipse.emf.ecore.uri_mapping`
            - Right-click and select `New > mapping`.
            - Source: `pathmap://UMLsec/<profilename>.uml`
            - Target: `platform:/plugin/carisma.profile.umlsec/<profilename>/profile/<profilename>.profile.uml`
        - `org.eclipse.papyrus.uml.extensionpoints.UMLProfile`
            - Right-click and select `New > profile`.
            - Name: `UMLsec.<profilename>`
            - Path: `pathmap://UMLsec/<profilename>.uml`
            - IconPath: Path to the CARiSMA logo file.
        - `org.eclipse.emf.ecore.generated_package`
            - URI: `http://www.umlsec.de/profiles/UMLsec/<profilename>.ecore`
            - Class: `carisma.profile.umlsec.<profilename>.<Profilename>Package`
            - GenModel: `profile/<profilename>.profile.genmodel`
3. Edit the `build.properties` file:
    ```
    source. = gen-src/,\
    src/
    output. = target/classes/
    bin.includes = META-INF/,\
    .,\
    plugin.xml,\
    profile/,\
    plugin.properties,\
    src/,\
    gen-src/
    src.includes = profile/
    ```
4. Import the project into the plugin directory of the CARiSMA tool project.
5. Edit `pom.xml` in the CARiSMA tool project and add:
    ```xml
    <module>plugins/carisma.profile.{profile_name}</module>
    ```
6. Build the CARiSMA tool project and run it as an Eclipse application.
