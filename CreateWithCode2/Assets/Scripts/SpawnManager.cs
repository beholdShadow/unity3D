using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SpawnManager : MonoBehaviour
{
    // Start is called before the first frame update
    public GameObject[] animals;
    private float leftX = -20;
    private float rightX = 20;
    private float Z = 20;

    private float startDelay = 2;
    private float spawnInterval = 1.5f;
    void Start()
    {
        InvokeRepeating("spawnAnimal", startDelay, spawnInterval);
    }

    // Update is called once per frame
    void Update()
    {

    }

    void spawnAnimal()
    {
        Vector3 pos = new Vector3(Random.Range(leftX, rightX), 0, Z);
        int index = Random.Range(0, animals.Length);
        Instantiate(animals[index], pos, animals[index].transform.rotation);
        
    }
}
