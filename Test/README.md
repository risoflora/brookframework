# How to do the tests?

Compile the test you want. Now, after generating its executable, open the terminal and run the script related to your system. On Linux:

```bash
./RunAllTests.sh
```

on Windows:

```cmd
RunAllTests.bat
```

# How to check if the test is OK?

The script will report the result of all tests, for example:

```
./Test_String OK
./Test_StringMap OK
./Test_Utils OK
./Sagui/Test_libsagui OK
Total: 4
```

If you do not get the message `OK` in front the test, then probably it failed.

**Note:** you need to install the Brook library before doing any test, because the most of them depends on it.

# How to report a bug when a test fails?

You have two options:

1. Open issue at [new issue](https://github.com/risoflora/brookframework/issues/new) page;
2. Fork the project, fix the bug and open a pull-request ([more about fork/PR](https://guides.github.com/activities/forking)).