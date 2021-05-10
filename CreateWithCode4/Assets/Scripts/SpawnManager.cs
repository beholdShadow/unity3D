using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SpawnManager : MonoBehaviour
{
    // Start is called before the first frame update
    public GameObject enemyPerfab;
    public GameObject powerUpPerfab;
    private float range = 9;
    public int enemyCount;
    public int waveNumber = 0;
    void Start()
    {
    }

    void SpawnEnemyWave(int num) {
        for (int i = 0; i < num; i++) {
            Instantiate(enemyPerfab, genarateRandomPos(), enemyPerfab.transform.rotation);
        }
    }
    Vector3 genarateRandomPos() {
        float x = Random.Range(-range, range);
        float z = Random.Range(-range, range);
        Vector3 pos = new Vector3(x, 0, z);
        return pos;
    }
    // Update is called once per frame
    void Update()
    {
        enemyCount = FindObjectsOfType<Enemy>().Length;
        if (enemyCount == 0)
        {
            SpawnEnemyWave(++waveNumber);
            Instantiate(powerUpPerfab, genarateRandomPos(), powerUpPerfab.transform.rotation);
        }
    }
}
