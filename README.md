# ARCAD Foundation Services

The ARCAD Foundation services propose a technical base build as an OSGi framework. Each service, function or API are embedded into a Bundle, even if some can be used into "plain java applications".

## Risks and Returns

This framework is based on the OSGi platform and the Restlet framework, it will confere the following main features *out of the box* :

* **Component oriented development**: highly modular, dynamic and extendable framework.
* **SOA**: The AFS framework is Service oriented, this mean that it include web-services (REST), and internal Service architecture (OSGi) and is compatible with a micro-service architecture (or into and Agent oriented architecture).
* **Compatible**: your project is compatible, even ''embedded'', with other projects developed with AFS. As far as possible, every feature and external tool included into the AFS deliveries are compatible with each other.
* **Secured**: Your technical base is stable and security questions are taken into account (with dedicated features), as long as licensing questions.
Up to date: AFS is maintained to the latest version of technology that do not compromise previous arguments.


## Download

Each release include a P2 repository which may be used a a target within Eclipse IDE.


## External dependencies

Two features of bundles are provided into the releases of AFS, one for the "server" part built on OSGi framework, and the other one for RCP development.
You can retrieve these dependencies from the **pom.xml** file from the root of this project and in the **target platform** defined in the /releng/platform folder.

### OSGi server

To be able to run the server part you will have to include the following dependencies:

* Bouncy Castle version 1.78.1.
* Apache Santuario XMLSec version 2.3.4
* Apache Commons bundles:
  * Commons FileUpload version 1.5
  * Commons Codec version 1.15
  * Commons DBUtils version 1.6
  * and Commons io version 2.13.0
* UnboundID LDAP SDK version 6.0.5
* Java Mail version 1.6.2
* HikariCP version 4.0.3 and the JDBC driver:
  * H2Database version 1.4.199
  * PostgreSQL version 42.5.5
* Groovy version 3.0.8
* and the Equinox OSGi framework, a version compatible with Java 8 and at least equal to version 4.19 is required. The required bundles are only the ones related to:
  * The Declarative Service.
  * The Event Admin Service.
  * The Equinox console.
 
### RCP Client

All the above dependencies are required, except the JDBC related ones and Groovy. You have to import Eclipse RCP version Neon or an above version compatible with Java 8.
