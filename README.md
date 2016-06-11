KVFS - Key-Value File System
====
What is KVFS and why must one adapt it?
----
KVFS uses state of the art standard technology, KV, which is developed and solely used by Valve. KVFS extends such format to allow mounting it through fuse.

Amongst the extensins of KVFS are:
        * Full disk encryption through the Base-64 technology
        * Accessing the file system without mounting
          * Such a feature allows standard tools such as grep to search the file system rather than use unstandard tools like find.

![Demo](https://github.com/Myrl/KVFS/raw/master/demo.gif "Demo")