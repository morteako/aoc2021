plugins {
    kotlin("jvm") version "1.5.0"
    application
}

group = "org.example"
version = "1.0-SNAPSHOT"

allprojects {
    repositories {
        mavenCentral()
    }
}

application {
    mainClass.set("aoc2021.app.AppKt")
}

tasks.test {
    useJUnitPlatform()
}

dependencies {
    implementation(kotlin("stdlib"))
    implementation("io.arrow-kt:arrow-core:1.0.0")

    testImplementation(kotlin("test"))
}